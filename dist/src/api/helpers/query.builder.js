'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _logicOperator = require('../domain/core/filter/logic.operator.enum');

var _logicOperator2 = _interopRequireDefault(_logicOperator);

var _filterOperator = require('../domain/core/filter/filter.operator.enum');

var _filterOperator2 = _interopRequireDefault(_filterOperator);

var _query = require('./query.parser');

var _query2 = _interopRequireDefault(_query);

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _defineProperty(obj, key, value) { if (key in obj) { Object.defineProperty(obj, key, { value: value, enumerable: true, configurable: true, writable: true }); } else { obj[key] = value; } return obj; }

var Op = _sequelize2.default.Op;

function buildQuery(model, requestQuery) {
    requestQuery = _query2.default.parseRequestSymbols(requestQuery);

    var nestedModels = getNestedModels(model);

    var query = buildWhereQuery(requestQuery.filter, model, nestedModels);
    query = _extends({}, query, buildProjections(requestQuery.projection));
    query = _extends({}, query, buildIncludes(requestQuery, model, model));
    //query = { ...query, ...buildAggregations(requestQuery) };
    query = _extends({}, query, buildGroupBy(requestQuery.groupBy, model, nestedModels));
    query = _extends({}, query, buildOrder(requestQuery.sort, model, nestedModels));

    query.limit = _query2.default.getLimit(requestQuery);
    query.offset = _query2.default.getOffset(requestQuery);

    console.log(query);

    return query;
}

function buildIncludes(requestQuery, model, originalModel) {
    var query = arguments.length > 3 && arguments[3] !== undefined ? arguments[3] : {};

    if (model.associations) {
        Object.keys(model.associations).forEach(function (key) {
            if (model.associations[key].target) {
                query.include = [];

                var associationInclude = {
                    model: model.associations[key].target,
                    as: key
                };
                associationInclude = _extends({}, associationInclude, buildProjections(requestQuery.projection, key));

                query.include.push(associationInclude);

                if (!originalModel || model.associations[key].target.name !== originalModel.name) {
                    var currentJoin = query.include[query.include.length - 1];
                    var childJoin = buildIncludes(requestQuery, model.associations[key].target, model, currentJoin);

                    query.include[query.include.length - 1] = _extends({}, currentJoin, childJoin);
                }
            }
        });
    }

    return query;
}

function buildProjections(projection) {
    var modelAlias = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : null;

    var projections = _query2.default.parseSeletor(projection);
    var query = {};

    if (projections.length === 0) {
        return query;
    }

    var projectionModels = {};

    for (var i = 0; i < projections.length; i++) {
        var modelName = getFieldModelName(projections[i]);
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
    var groupByFields = _query2.default.parseSeletor(groupBy);
    var query = {};

    for (var i = 0; i < groupByFields.length; i++) {
        var nestedModel = getNestedModel(nestedModels, groupByFields[i]);
        var field = getSignificantField(groupByFields[i]);

        query.group = query.group || [];

        if (nestedModel === null) {
            query.group.push([model, field]);
        } else {
            query.group.push([nestedModel, field]);
        }
    }

    return query;
}

function buildOrder(sort, model, nestedModels) {
    var orderFields = _query2.default.parseSortOrder(sort);
    var query = {};

    for (var i = 0; i < orderFields.length; i++) {
        var nestedModel = getNestedModel(nestedModels, orderFields[i].field);
        var field = getSignificantField(orderFields[i].field);

        query.order = query.order || [];

        if (nestedModel === null) {
            query.order.push([model, ield, orderFields[i].sortOrder.order]);
        } else {
            query.order.push([nestedModel, field, orderFields[i].sortOrder.order]);
        }
    }

    return query;
}

function buildWhereQuery(filter, model, nestedModels) {
    var currentExpression = _query2.default.parseFilterExpressions(filter);
    var query = {};

    while (currentExpression !== null && Object.keys(currentExpression).length > 0) {

        if (currentExpression.filterField !== null) {
            var fieldModel = getFieldModel(model, nestedModels, currentExpression.filterField.field);

            query.where = query.where || {};

            if (currentExpression.logicOperator && _logicOperator2.default.OR.name === currentExpression.logicOperator.name) {
                var conjunctionQuery = [];

                do {
                    conjunctionQuery.push(buildPredicate(currentExpression.filterField, fieldModel));
                    currentExpression = currentExpression.filterExpression;
                } while (currentExpression !== null && _logicOperator2.default.OR.name === currentExpression.logicOperator.name);

                if (currentExpression !== null && currentExpression.filterField !== null) {
                    conjunctionQuery.push(buildPredicate(currentExpression.filterField, fieldModel));
                }

                query.where[Op.or] = query.where[Op.or] || [];
                query.where[Op.or].concat(conjunctionQuery);
            } else {
                query.where = _extends({}, query.where, buildPredicate(currentExpression.filterField, fieldModel));
            }
        }

        currentExpression = currentExpression.filterExpression;
    }

    return query;
}

function getFieldModel(model, nestedModels, field) {
    var nestedModel = getNestedModel(nestedModels, field);

    if (nestedModel === null) {
        return model;
    }

    return nestedModel.model;
}

function getNestedModel(nestedModels, field) {
    var nestedModel = null;
    var fieldModelName = getFieldModelName(field);

    if (fieldModelName !== null) {
        var nestedModelFound = nestedModels.find(function (nested) {
            return nested.as === fieldModelName;
        });
        if (nestedModelFound) {
            nestedModel = nestedModelFound;
        }
    }

    return nestedModel;
}

function getNestedModels(model) {
    var models = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : [];

    if (model.associations) {
        Object.keys(model.associations).forEach(function (key) {
            if (model.associations[key].target) {
                models.push({
                    model: model.associations[key].target,
                    as: key
                });
                models.concat(getNestedModels(model.associations[key].target, models));
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
    var columnField = getColumnField(filterField.field, getSignificantField(filterField.field), fieldModel);

    switch (filterField.filterOperator.name) {
        case _filterOperator2.default.IN.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.in, filterField.value.split(',').map(function (val) {
                return val.trim();
            })));

        case _filterOperator2.default.OU.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.notIn, filterField.value.split(',').map(function (val) {
                return val.trim();
            })));

        case _filterOperator2.default.GE.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.ge, filterField.value));

        case _filterOperator2.default.GT.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.gt, filterField.value));

        case _filterOperator2.default.LE.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.le, filterField.value));

        case _filterOperator2.default.LT.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.lt, filterField.value));

        case _filterOperator2.default.NE.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.ne, filterField.value));

        case _filterOperator2.default.LK.name:
            return _defineProperty({}, columnField, _defineProperty({}, Op.iLike, '%' + filterField.value + '%'));

        case _filterOperator2.default.EQ.name:
        default:
            return _defineProperty({}, columnField, _defineProperty({}, Op.eq, filterField.value));
    }
}

function getAllModelFields(model) {
    var fieldNames = [];
    var modelFields = Object.keys(model.fieldRawAttributesMap);

    for (var i = 0; i < modelFields.length; i++) {
        var modelField = modelFields[i];
        fieldNames.push(model.fieldRawAttributesMap[modelField].fieldName);
    }

    return fieldNames;
}

function getColumnField(fullPathField, field, fieldModel) {
    var modelFields = Object.keys(fieldModel.fieldRawAttributesMap);
    var originalColumnField = '';

    for (var i = 0; i < modelFields.length; i++) {
        var modelField = modelFields[i];

        if (fieldModel.fieldRawAttributesMap[modelField].fieldName === field) {
            originalColumnField = modelField;
            break;
        }
    }

    var splittedFullPathField = fullPathField.split('.');
    splittedFullPathField[splittedFullPathField.length - 1] = originalColumnField;

    if (splittedFullPathField.length === 1) {
        splittedFullPathField.splice(0, 0, fieldModel.name);
    }

    return '$' + splittedFullPathField.join('.') + '$';
}

function getFieldModelName(field) {
    var fields = field.split('.');

    if (fields.length === 1) {
        return null;
    }

    return fields[fields.length - 2];
}

function getSignificantField(field) {
    var fields = field.split('.');
    return fields[fields.length - 1];
}

exports.default = { buildQuery: buildQuery };
module.exports = exports['default'];
//# sourceMappingURL=query.builder.js.map
