import LogicOperator from '../../domain/core/filter/logic.operator.enum';
import AggregateFunction from '../../domain/core/filter/aggregate.function.enum';
import FilterOperator from '../../domain/core/filter/filter.operator.enum';
import requestParser from './request.parser';
import Sequelize from 'sequelize';
import _ from 'lodash';

const Op = Sequelize.Op;
const ROOT_MODEL = '_.'; 

function buildQuery(model, requestQuery) {
    requestQuery = requestParser.parseSymbols(requestQuery);
    
    let nestedModels = getNestedModels(model);
    let query = buildWhere(requestQuery.filter, model, nestedModels);
    
    if (requestParser.hasValidAggregateFunction(requestQuery)) {
        query = { ...query, ...buildAggregations(requestQuery, model, nestedModels) };
        query = { ...query, ...buildGroupBy(requestQuery.groupBy, model, nestedModels) };

    } else {
        query = { ...query, ...buildProjections(requestQuery.projection) };
    }
    
    query = { ...query, ...buildIncludes(requestQuery, model) };
    query = { ...query, ...buildOrder(requestQuery.sort, nestedModels, model) };
    
    query.limit = requestParser.getLimit(requestQuery);
    query.offset = requestParser.getOffset(requestQuery);

    query.raw = true;  
    query.mapToModel = true;
    query.nest = true;

    return query;
}

function buildIncludes(requestQuery, model, rootModel = model) {
    let query = {};
  
    if (!model.associations)  {
        return query;
    }
  
    const includes = [];
    const stack = [{ model, key: null }];
  
    while (stack.length > 0) {
        const { model, key } = stack.pop();
  
        Object.keys(model.associations).forEach((childKey) => {
            
            const association = model.associations[childKey];
            if (!association.target) {
                return;
            }
  
            let join = { model: association.target, as: childKey };
            if (!requestParser.hasValidAggregateFunction(requestQuery)) {
                join = { ...join, ...buildProjections(requestQuery.projection, childKey) };
            }
            
            setJoinIncludes(key, includes, join);

            if (association.target.name !== rootModel.name)  {
                stack.push({ model: association.target, key: childKey });
            }
        });
    }
  
    query.include = includes;
    return query;
}

function setJoinIncludes(key, includes, join) {
    if (key) {
        includes[includes.length - 1].include = includes[includes.length - 1].include || [];
        includes[includes.length - 1].include.push(join);
    } else {
        includes.push(join);
    }
}

function buildAggregations(requestQuery, model, nestedModels) {
    let sum = requestParser.parseSeletor(requestQuery.sum);
    let avg = requestParser.parseSeletor(requestQuery.avg);
    let count = requestParser.parseSeletor(requestQuery.count);
    let countDistinct = requestParser.parseSeletor(requestQuery.countDistinct);
    let groupBy = requestParser.parseSeletor(requestQuery.groupBy);
    
    let query = {};

    if (sum.length === 0 && avg.length === 0 && count.length === 0 
            && countDistinct.length === 0 && groupBy.length === 0) {

        return query;
    }

    let aggregation = buildFunctionProjection(sum, AggregateFunction.SUM, model, nestedModels);
    aggregation = aggregation.concat(buildFunctionProjection(avg, AggregateFunction.AVG, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(count, AggregateFunction.COUNT, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(countDistinct, AggregateFunction.COUNT_DISTINCT, model, nestedModels));
    aggregation = aggregation.concat(buildFunctionProjection(groupBy, AggregateFunction.GROUP_BY, model, nestedModels));

    query.attributes = aggregation;

    return query;
}

function buildFunctionProjection(functionFields, aggregateFunction, model, nestedModels) {
    let aggregation = [];

    for (const functionField of functionFields) {
        let nestedModel = getFieldModel(model, nestedModels, functionField);
        let columnField = getLiteralField(functionField, nestedModel);

        if (aggregateFunction.name === AggregateFunction.GROUP_BY.name) {
            aggregation.push(
                [ Sequelize.literal(columnField), functionField ]
            ); 

        } else {
            let sqlFunctionCompiled = _.template(aggregateFunction.sqlFunctionTemplate);
            aggregation.push(
                [ Sequelize.literal(sqlFunctionCompiled({ columnField })), functionField ]
            );            
        }
    }

    return aggregation;
}

function buildProjections(projection, modelAlias = ROOT_MODEL) {
    let projections = requestParser.parseSeletor(projection);
    let query = {};

    if (projections.length === 0) {
        return query;
    }

    let projectionModels = {};

    for (const projection of projections) {
        let modelName = getFieldModelName(projection);
        projectionModels[modelName] = projectionModels[modelName] || [];
        projectionModels[modelName].push(getSignificantField(projection));
    }

    if (projectionModels[modelAlias]) {
        query.attributes = projectionModels[modelAlias];
    
    } else if (Object.keys(projectionModels).length > 0) {
        query.attributes = [];
    }

    return query;
}

function buildGroupBy(groupBy, model, nestedModels) {
    let groupByFields = requestParser.parseSeletor(groupBy);
    let query = {};

    for (const groupByField of groupByFields) {
        let nestedModel = getNestedModel(nestedModels, groupByField);

        query.group = query.group || [];

        if (nestedModel === null) {
            query.group.push([ getLiteralLastField(groupByField, model) ]);
        
        } else {
            query.group.push([ getLiteralLastField(groupByField, nestedModel.model) ]);
        }
    }

    return query;
}

function buildOrder(sort, nestedModels, model) {
    let orderFields = requestParser.parseSortOrder(sort);
    let query = {};

    for (const orderField of orderFields) {
        let splittedField = orderField.field.split('.');
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
            order.push(orderField.sortOrder.order)

            query.order.push([ order ]);

        } else {
            query.order.push([ getModelColumnField(field, model), orderField.sortOrder.order ]);
        }
    }

    return query;
}

function buildWhere(filter, model, nestedModels) {
    let currentExpression = requestParser.parseFilterExpressions(filter);
    let query = {};
    
    while (currentExpression !== null && Object.keys(currentExpression).length > 0) {
        
        if (currentExpression.filterField !== null) {
            query.where = query.where || {};

            if (currentExpression.logicOperator && LogicOperator.OR.name === currentExpression.logicOperator.name) {
                let conjunctionQuery = [];

                getOrRestrictions(conjunctionQuery, model, nestedModels, currentExpression);

                query.where[Op.and] = query.where[Op.and] || [];
                query.where[Op.and].push({ [Op.or]: conjunctionQuery });

            } else {
                let fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
                query.where[Op.and] = query.where[Op.and] || [];
                query.where[Op.and].push(buildWhereCondition(currentExpression.filterField, fieldModel)); 
            }
        }
        
        currentExpression = currentExpression.filterExpression;
    }

    return query;
}

function getOrRestrictions(conjunctionQuery, model, nestedModels, currentExpression) {
    do {
        let fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
        conjunctionQuery.push(buildWhereCondition(currentExpression.filterField, fieldModel));
        currentExpression = currentExpression.filterExpression;
    
    } while (currentExpression !== null && currentExpression.logicOperator 
            && LogicOperator.OR.name === currentExpression.logicOperator.name);
    
    if (currentExpression !== null && currentExpression.filterField !== null) {
        let fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);
        conjunctionQuery.push(buildWhereCondition(currentExpression.filterField, fieldModel));
    }
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

    for (const modelField of modelFields) {
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

    for (const modelField of modelFields) {
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
        return ROOT_MODEL;
    }

    return fields[fields.length - 2];
}

function getSignificantField(field) {
    let fields = field.split('.');
    return fields[fields.length - 1];
}

export default { buildQuery };
