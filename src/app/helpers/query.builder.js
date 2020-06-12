import LogicOperator from '../domain/core/filter/logic.operator.enum';
import FilterOperator from '../domain/core/filter/filter.operator.enum';
import queryParser from './query.parser';
import Sequelize from 'sequelize';

const Op = Sequelize.Op;

function buildQuery(model, requestQuery) {
    requestQuery = queryParser.parseRequestSymbols(requestQuery);
    
    let nestedModels = getNestedModels(model);

    let query = buildWhereQuery(requestQuery.filter, model, nestedModels);
    query = { ...query, ...buildProjections(requestQuery.projection) };
    query = { ...query, ...buildIncludes(requestQuery, model, model) };
    //query = { ...query, ...buildAggregations(requestQuery) };
    query = { ...query, ...buildGroupBy(requestQuery.groupBy, model, nestedModels) };
    query = { ...query, ...buildOrder(requestQuery.sort, model, nestedModels) };
    
    query.limit = queryParser.getLimit(requestQuery);
    query.offset = queryParser.getOffset(requestQuery);

    console.log(query);

    return query;
}

function buildIncludes(requestQuery, model, originalModel, query = {}) {
    if (model.associations) {
        Object.keys(model.associations).forEach((key) => {
            if (model.associations[key].target) {
                query.include = [];

                let associationInclude = {
                    model: model.associations[key].target,
                    as: key,
                };
                associationInclude = { ...associationInclude, ...buildProjections(requestQuery.projection, key) };

                query.include.push(associationInclude);

                if (!originalModel || (model.associations[key].target.name !== originalModel.name)) {
                    let currentJoin = query.include[query.include.length - 1];    
                    let childJoin = buildIncludes(requestQuery, model.associations[key].target, model, currentJoin);
                                    
                    query.include[query.include.length - 1] = { ...currentJoin, ...childJoin };
                }
            }
        });
    }

    return query;
}

function buildProjections(projection, modelAlias = null) {
    let projections = queryParser.parseSeletor(projection);
    let query = {};

    if (projections.length === 0) {
        return query;
    }

    let projectionModels = {};

    for (let i = 0; i < projections.length; i++) {
        let modelName = getFieldModelName(projections[i]);
        projectionModels[modelName] = projectionModels[modelName] || [];
        projectionModels[modelName].push(getSignificantField(projections[i]));
    }

    if (projectionModels[modelAlias]) {
        query.attributes = projectionModels[modelAlias];
    
    } else if (Object.keys(projectionModels).length > 0) {
        query.attributes = [];
    }

    return query;
}

function buildGroupBy(groupBy, model, nestedModels) {
    let groupByFields = queryParser.parseSeletor(groupBy);
    let query = {};

    for (let i = 0; i < groupByFields.length; i++) {
        let nestedModel = getNestedModel(nestedModels, groupByFields[i]);
        let field = getSignificantField(groupByFields[i]);

        query.group = query.group || [];
        
        if (nestedModel === null) {
            query.group.push([ model, field ]);
        } else {
            query.group.push([ nestedModel, field ]);
        }
    }

    return query;
}

function buildOrder(sort, model, nestedModels) {
    let orderFields = queryParser.parseSortOrder(sort);
    let query = {};

    for (let i = 0; i < orderFields.length; i++) {
        let nestedModel = getNestedModel(nestedModels, orderFields[i].field);
        let field = getSignificantField(orderFields[i].field);

        query.order = query.order || [];
        
        if (nestedModel === null) {
            query.order.push([ model, ield, orderFields[i].sortOrder.order ]);
        } else {
            query.order.push([ nestedModel, field, orderFields[i].sortOrder.order ]);
        }
    }

    return query;
}

function buildWhereQuery(filter, model, nestedModels) {
    let currentExpression = queryParser.parseFilterExpressions(filter);
    let query = {};
    
    while (currentExpression !== null && Object.keys(currentExpression).length > 0) {
        
        if (currentExpression.filterField !== null) {
            let fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);

            query.where = query.where || {};

            if (currentExpression.logicOperator && LogicOperator.OR.name === currentExpression.logicOperator.name) {
                let conjunctionQuery = [];

                do {
                    conjunctionQuery.push(buildPredicate(currentExpression.filterField, fieldModel));
                    currentExpression = currentExpression.filterExpression;
                
                } while (currentExpression !== null && LogicOperator.OR.name === currentExpression.logicOperator.name);
                
                if (currentExpression !== null && currentExpression.filterField !== null) {
                    conjunctionQuery.push(buildPredicate(currentExpression.filterField, fieldModel));
                }
                
                query.where[Op.or] = query.where[Op.or] || [];
                query.where[Op.or].concat(conjunctionQuery);

            } else {
                query.where = { ...query.where, ...buildPredicate(currentExpression.filterField, fieldModel) };
            }
        }
        
        currentExpression = currentExpression.filterExpression;
    }

    return query;
}

function getFieldModel(model, nestedModels, field) {
    let nestedModel = getNestedModel(nestedModels, field);

    if (nestedModel === null) {
        return model;
    }

    return nestedModel.model;
}

function getNestedModel(nestedModels, field) {
    let nestedModel = null;    
    let fieldModelName = getFieldModelName(field);

    if (fieldModelName !== null) {
        let nestedModelFound = nestedModels.find(nested => nested.as === fieldModelName);
        if (nestedModelFound) {
            nestedModel = nestedModelFound;
        }
    }

    return nestedModel;
}

function getNestedModels(model, models = []) {
    if (model.associations) {
        Object.keys(model.associations).forEach((key) => {
            if (model.associations[key].target) {
                models.push({
                    model: model.associations[key].target,
                    as: key
                });
                models.concat(getNestedModels(model.associations[key].target, models))                
            }
        });
    }
    return models;
}

function buildPredicate(filterField, fieldModel) {
    if ('null' === filterField.value) {
        filterField.value = null;
    }

    return buildWhereCondition(filterField, fieldModel);
}    

function buildWhereCondition(filterField, fieldModel) {
    let columnField = getColumnField(filterField.field, getSignificantField(filterField.field), fieldModel);

    switch (filterField.filterOperator.name) {
        case FilterOperator.IN.name:
            return {
                [columnField]: {
                    [Op.in]: filterField.value.split(',').map(val => val.trim())
                }    
            }
        
        case FilterOperator.OU.name:
            return {
                [columnField]: {
                    [Op.notIn]: filterField.value.split(',').map(val => val.trim())
                }    
            }
        
        case FilterOperator.GE.name:
            return {
                [columnField]: {
                    [Op.ge]: filterField.value
                }    
            }
        
        case FilterOperator.GT.name:
            return {
                [columnField]: {
                    [Op.gt]: filterField.value
                }    
            }
        
        case FilterOperator.LE.name:
            return {
                [columnField]: {
                    [Op.le]: filterField.value
                }    
            }
        
        case FilterOperator.LT.name:
            return {
                [columnField]: {
                    [Op.lt]: filterField.value
                }    
            }
        
        case FilterOperator.NE.name:
            return {
                [columnField]: {
                    [Op.ne]: filterField.value
                }    
            }
        
        case FilterOperator.LK.name:
            return {
                [columnField]: {
                    [Op.iLike]: '%' + filterField.value + '%'
                }    
            }
            
        case FilterOperator.EQ.name:
        default:
            return {
                [columnField]: {
                    [Op.eq]: filterField.value
                }    
            }
    }
}

function getAllModelFields(model) {
    let fieldNames = [];
    let modelFields = Object.keys(model.fieldRawAttributesMap);

    for (let i = 0; i < modelFields.length; i++) {
        let modelField = modelFields[i];
        fieldNames.push(model.fieldRawAttributesMap[modelField].fieldName);
    }

    return fieldNames;
}

function getColumnField(fullPathField, field, fieldModel) {
    let modelFields = Object.keys(fieldModel.fieldRawAttributesMap);
    let originalColumnField = '';

    for (let i = 0; i < modelFields.length; i++) {
        let modelField = modelFields[i];

        if (fieldModel.fieldRawAttributesMap[modelField].fieldName === field) {
            originalColumnField = modelField;
            break;
        }
    }

    let splittedFullPathField = fullPathField.split('.');
    splittedFullPathField[splittedFullPathField.length - 1] = originalColumnField;

    if (splittedFullPathField.length === 1) {
        splittedFullPathField.splice(0, 0, fieldModel.name);
    }

    return '$' + splittedFullPathField.join('.') + '$';
}

function getFieldModelName(field) {
    let fields = field.split('.');
    
    if (fields.length === 1) {
        return null;
    }

    return fields[fields.length - 2];
}

function getSignificantField(field) {
    let fields = field.split('.');
    return fields[fields.length - 1];
}

export default { buildQuery };