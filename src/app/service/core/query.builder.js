import LogicOperator from '../../domain/core/filter/logic.operator.enum';
import AggregateFunction from '../../domain/core/filter/aggregate.function.enum';
import FilterOperator from '../../domain/core/filter/filter.operator.enum';
import queryParser from './query.parser';
import Sequelize from 'sequelize';

const Op = Sequelize.Op;

function buildQuery(model, requestQuery) {
    requestQuery = queryParser.parseRequestSymbols(requestQuery);
    
    let nestedModels = getNestedModels(model);
    let query = buildWhere(requestQuery.filter, model, nestedModels);
    
    if (queryParser.hasValidAggregateFunction(requestQuery)) {
        query = { ...query, ...buildAggregations(requestQuery, model, nestedModels) };
        query = { ...query, ...buildGroupBy(requestQuery.groupBy, model, nestedModels) };

    } else {
        query = { ...query, ...buildProjections(requestQuery.projection) };
    }
    
    query = { ...query, ...buildIncludes(requestQuery, model) };
    query = { ...query, ...buildOrder(requestQuery.sort, nestedModels, model) };
    
    query.limit = queryParser.getLimit(requestQuery);
    query.offset = queryParser.getOffset(requestQuery);

    query.raw = true;  
    query.mapToModel = true;
    query.nest = true;

    return query;
}

function buildIncludes(requestQuery, model, rootModel = model, query = {}) {
    if (model.associations) {
        Object.keys(model.associations).forEach((key) => {
            if (model.associations[key].target) {
                query.include = [];

                let associationInclude = {
                    model: model.associations[key].target,
                    as: key,
                };

                if (queryParser.hasValidAggregateFunction(requestQuery)) {
                    associationInclude.attributes = [];

                } else {
                    associationInclude = { ...associationInclude, ...buildProjections(requestQuery.projection, key) };
                }

                query.include.push(associationInclude);

                if (model.associations[key].target.name !== rootModel.name) {
                    let lastIndex = query.include.length - 1;
                    let currentJoin = query.include[lastIndex];    
                    let childJoin = buildIncludes(requestQuery, model.associations[key].target, rootModel, currentJoin);
                    
                    query.include[lastIndex] = { ...currentJoin, ...childJoin };
                }
            }
        });
    }

    return query;
}

function buildAggregations(requestQuery, model, nestedModels) {
    let sum = queryParser.parseSeletor(requestQuery.sum);
    let avg = queryParser.parseSeletor(requestQuery.avg);
    let count = queryParser.parseSeletor(requestQuery.count);
    let countDistinct = queryParser.parseSeletor(requestQuery.countDistinct);
    let groupBy = queryParser.parseSeletor(requestQuery.groupBy);
    
    let query = {};

    if (sum.length === 0 && avg.length === 0 && count.length === 0 
            && countDistinct.length === 0 && groupBy.length === 0) {

        return query;
    }

    let aggregation = buildFunctionProjection(sum, AggregateFunction.SUM.function, model, nestedModels);
    aggregation = aggregation.concat(buildFunctionProjection(avg, AggregateFunction.AVG.function, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(count, AggregateFunction.COUNT.function, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(countDistinct, AggregateFunction.COUNT_DISTINCT.function, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(groupBy, AggregateFunction.GROUP_BY.function, model, nestedModels));

    query.attributes = aggregation;

    return query;
}

function buildFunctionProjection(functionFields, sqlFunction, model, nestedModels) {
    let aggregation = [];

    for (let i = 0; i < functionFields.length; i++) {
        let nestedModel = getFieldModel(model, nestedModels, functionFields[i]);
        let columnField = getLiteralField(functionFields[i], nestedModel);

        if (sqlFunction === AggregateFunction.COUNT_DISTINCT.function) {
            aggregation.push(
                [ Sequelize.literal(sqlFunction + '(' + columnField + '))'), functionFields[i] ]
            );

        } else if (sqlFunction === AggregateFunction.GROUP_BY.function) {
            aggregation.push(
                [ Sequelize.literal(columnField), functionFields[i] ]
            ); 

        } else {
            aggregation.push(
                [ Sequelize.literal(sqlFunction + '(' + columnField + ')'), functionFields[i] ]
            );            
        }
    }

    return aggregation;
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

        query.group = query.group || [];

        if (nestedModel === null) {
            query.group.push([ getLiteralLastField(groupByFields[i], model) ]);
        
        } else {
            query.group.push([ getLiteralLastField(groupByFields[i], nestedModel.model) ]);
        }
    }

    return query;
}

function buildOrder(sort, nestedModels, model) {
    let orderFields = queryParser.parseSortOrder(sort);
    let query = {};

    for (let i = 0; i < orderFields.length; i++) {
        let splittedField = orderFields[i].field.split('.');
        let field = splittedField[splittedField.length - 1];

        query.order = query.order || [];
        
        if (splittedField.length > 1) {
            let order = [];
            let lastNestedModel = null;

            for (let j = 0; j < splittedField.length - 1; j++) {
                lastNestedModel = getNestedModelByName(nestedModels, splittedField[j]);
                order.push(lastNestedModel);
            }
            
            order.push(getModelColumnField(field, lastNestedModel.model));
            order.push(orderFields[i].sortOrder.order)

            query.order.push([ order ]);

        } else {
            query.order.push([ getModelColumnField(field, model), orderFields[i].sortOrder.order ]);
        }
    }

    return query;
}

function buildWhere(filter, model, nestedModels) {
    let currentExpression = queryParser.parseFilterExpressions(filter);
    let query = {};
    
    while (currentExpression !== null && Object.keys(currentExpression).length > 0) {
        
        if (currentExpression.filterField !== null) {
            let fieldModel = model;
            query.where = query.where || {};

            if (currentExpression.logicOperator && LogicOperator.OR.name === currentExpression.logicOperator.name) {
                let conjunctionQuery = [];

                do {
                    fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
                    conjunctionQuery.push(buildWhereCondition(currentExpression.filterField, fieldModel));
                    currentExpression = currentExpression.filterExpression;
                
                } while (currentExpression !== null && currentExpression.logicOperator 
                        && LogicOperator.OR.name === currentExpression.logicOperator.name);
                
                if (currentExpression !== null && currentExpression.filterField !== null) {
                    fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
                    conjunctionQuery.push(buildWhereCondition(currentExpression.filterField, fieldModel));
                }

                query.where[Op.and] = query.where[Op.and] || [];
                query.where[Op.and].push({ [Op.or]: conjunctionQuery });

            } else {
                fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
                query.where[Op.and] = query.where[Op.and] || [];
                query.where[Op.and].push(buildWhereCondition(currentExpression.filterField, fieldModel)); 
            }
        }
        
        currentExpression = currentExpression.filterExpression;
    }

    return query;
}

function buildWhereCondition(filterField, fieldModel) {
    let columnField = getWhereField(filterField.field, getSignificantField(filterField.field), fieldModel);
    
    if ('null' === filterField.value) {
        filterField.value = null;
    }

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

function getWhereField(fullPathField, field, fieldModel) {
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

function getFieldModel(model, nestedModels, field) {
    let nestedModel = getNestedModel(nestedModels, field);

    if (nestedModel === null) {
        return model;
    }

    return nestedModel.model;
}

function getNestedModelByName(nestedModels, fieldModelName) {
    let nestedModel = null;    

    if (fieldModelName !== null) {
        let nestedModelFound = nestedModels.find(nested => nested.as === fieldModelName);
       
        if (nestedModelFound) {
            nestedModel = nestedModelFound;
        }
    }

    return nestedModel;
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
                models = models.concat(getNestedModels(model.associations[key].target, models))                
            }
        });
    }
    return models;
} 

function getModelColumnField(fullPathField, fieldModel) {
    let modelFields = Object.keys(fieldModel.fieldRawAttributesMap);
    let splittedField = fullPathField.split('.');
    let fieldName = splittedField[splittedField.length - 1];
    let columnField = null;

    for (let i = 0; i < modelFields.length; i++) {
        let modelField = modelFields[i];

        if (fieldModel.fieldRawAttributesMap[modelField].fieldName === fieldName) {
            columnField = modelField;
            break;
        }
    }

    if (columnField !== null) {
        splittedField[splittedField.length - 1] = columnField;
    }

    return splittedField.join('.');
}

function getLiteralLastField(field, model) {
    let fields = field.split('.');
    let columnField = getModelColumnField(fields[fields.length - 1], model);
    fields[fields.length - 1] = columnField;

    if (fields.length === 1) {
        return model.name + '.' + fields.join('.');
    }
    
    return fields.join('.');
}

function getLiteralField(field, model) {
    let fields = field.split('.');
    let literalField = '';
    let columnField = getModelColumnField(fields[fields.length - 1], model);

    if (fields.length === 1 && model) {
        literalField = '"' + model.name + '"."' + columnField + '"';
    
    }  else if (fields.length === 2) {
        literalField = '"' + fields[0] + '"."' + columnField + '"';
    
    } else {
        literalField += '"';

        for (let i = 0; i < fields.length; i++) {
            if (i < (fields.length - 2)) {
                literalField += fields[i] + '->';

            } else if (i === (fields.length - 2)) {
                literalField += fields[i] + '"."';
            
            } else {
                literalField += columnField + '"';
            }
        }
    }
 
    return literalField;
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
