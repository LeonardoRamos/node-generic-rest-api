'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _logicOperator = require('../domain/core/filter/logic.operator.enum');

var _logicOperator2 = _interopRequireDefault(_logicOperator);

var _filterOperator = require('../domain/core/filter/filter.operator.enum');

var _filterOperator2 = _interopRequireDefault(_filterOperator);

var _sortOrder = require('../domain/core/filter/sort.order.enum');

var _sortOrder2 = _interopRequireDefault(_sortOrder);

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var DEFAULT_OFFSET = 0;
var DEFAULT_LIMIT = 20;
var MAX_LIMIT = 100;

function getOffset(requestQuery) {
    if (requestQuery.offset === undefined || requestQuery.offset === null) {
        return DEFAULT_OFFSET;
    }

    return requestQuery.offset;
}

function getLimit(requestQuery) {
    var limit = requestQuery.limit;
    if (limit === undefined || limit === null) {
        limit = DEFAULT_LIMIT;
    } else if (limit > MAX_LIMIT) {
        limit = MAX_LIMIT;
    }

    return limit;
}

function parseSeletor(seletor) {
    if (!seletor) {
        return [];
    }

    return seletor.split(',').map(function (attr) {
        return attr.trim();
    });
}

function parseSortOrder(sort) {
    if (!sort) {
        return [];
    }

    var orderFields = [];
    var fields = sort.split(',').map(function (attr) {
        return attr.trim();
    });

    for (var i = 0; i < fields.length; i++) {
        var filterOrder = fields[i].split('=');
        orderFields.push({
            field: filterOrder[0].trim(),
            sortOrder: _sortOrder2.default.getSortOrder(filterOrder[1].trim())
        });
    }

    return orderFields;
}

function parseRequestSymbols(requestQuery) {
    if (requestQuery.filter) {
        requestQuery.filter = requestQuery.filter.split('[').join('').split(']').join('');
        requestQuery.filter = requestQuery.filter.split(_logicOperator2.default.AND.operatorAlias).join(_logicOperator2.default.AND.operator);
        requestQuery.filter = requestQuery.filter.split(_logicOperator2.default.OR.operatorAlias).join(_logicOperator2.default.OR.operator);
        requestQuery.filter = parseFilterOperators(requestQuery.filter);
    }

    if (requestQuery.projection) {
        requestQuery.projection = requestQuery.projection.split('[').join('').split(']').join('');
    }

    if (requestQuery.sum) {
        requestQuery.sum = requestQuery.sum.split('[').join('').split(']').join('');
    }

    if (requestQuery.avg) {
        requestQuery.avg = requestQuery.avg.split('[').join('').split(']').join('');
    }

    if (requestQuery.count) {
        requestQuery.count = requestQuery.count.split('[').join('').split(']').join('');
    }

    if (requestQuery.countDistinct) {
        requestQuery.countDistinct = requestQuery.countDistinct.split('[').join('').split(']').join('');
    }

    if (requestQuery.groupBy) {
        requestQuery.groupBy = requestQuery.groupBy.split('[').join('').split(']').join('');
    }

    if (requestQuery.sort) {
        requestQuery.sort = requestQuery.sort.split('[').join('').split(']').join('');
    }

    return requestQuery;
}

function parseFilterOperators(filter) {
    var simpleCharOperator = [_filterOperator2.default.EQ, _filterOperator2.default.GT, _filterOperator2.default.LT];

    Object.keys(_filterOperator2.default).forEach(function (filterOperator) {
        if (!simpleCharOperator.includes(_filterOperator2.default[filterOperator])) {
            filter = filter.split(_filterOperator2.default[filterOperator].operatorCommonAlias).join(_filterOperator2.default[filterOperator].parseableOperator);
            filter = filter.split(_filterOperator2.default[filterOperator].operatorAlias).join(_filterOperator2.default[filterOperator].parseableOperator);
        }
    });

    simpleCharOperator.forEach(function (simpleCharfilterOperator) {
        filter = filter.split(simpleCharfilterOperator.operatorCommonAlias).join(simpleCharfilterOperator.parseableOperator);
        filter = filter.split(simpleCharfilterOperator.operatorAlias).join(simpleCharfilterOperator.parseableOperator);
    });

    return filter;
}

function parseFilterExpressions(expressionString) {
    var currentExpression = {};

    if (expressionString === null) {
        return currentExpression;
    }

    var initialExpression = currentExpression;
    var word = '';

    for (var i = 0; expressionString && i < expressionString.length; i++) {

        if (expressionString.charAt(i) != '_') {
            word += expressionString.charAt(i);
        } else {
            var logicOperator = processOperator(expressionString, i);

            if (logicOperator != null) {
                currentExpression.logicOperator = logicOperator;
                currentExpression.filterField = parseFilterField(word.trim());
                currentExpression = processNewExpressionNode(currentExpression);

                i += logicOperator.getOperator().length - 1;
                word = '';
            } else {
                word += expressionString.charAt(i);
            }
        }
    }

    currentExpression.filterField = parseFilterField(word.trim());
    currentExpression = processNewExpressionNode(currentExpression);

    return initialExpression;
}

function parseFilterField(logicExpression) {
    var filterField = {};

    if (logicExpression == null || '' === logicExpression) {
        return null;
    }

    var word = '';

    for (var i = 0; i < logicExpression.length; i++) {

        if (logicExpression.charAt(i) !== '|') {
            word += logicExpression.charAt(i);
        } else {
            var filterOperator = getComparisonOperator(logicExpression, i);

            filterField.field = word.trim();
            filterField.filterOperator = filterOperator;

            i += filterOperator.parseableOperator.length - 1;
            word = '';
        }
    }

    filterField.value = word.trim();

    return filterField;
}

function getComparisonOperator(logicExpression, index) {
    var operation = '';
    var appendOperation = true;

    do {
        operation += logicExpression.charAt(index);
        index++;

        if (index >= logicExpression.length || logicExpression.charAt(index) == '|') {

            appendOperation = false;

            if (index < logicExpression.length && logicExpression.charAt(index) == '|') {
                operation += logicExpression.charAt(index);
            }
        }
    } while (appendOperation);

    return _filterOperator2.default.getFilterOperator(operation.trim());
}

function processNewExpressionNode(currentExpression) {
    if (currentExpression.logicOperator !== null) {
        currentExpression.filterExpression = {};
        currentExpression = currentExpression.filterExpression;
    }

    return currentExpression;
}

function processOperator(expressionString, index) {
    var logicOperatorText = '';
    var appendOperation = true;

    do {
        logicOperatorText += expressionString.charAt(index);
        index++;

        if (index >= expressionString.length || expressionString.charAt(index) == '_') {

            appendOperation = false;

            if (index < expressionString.length && expressionString.charAt(index) == '_') {
                logicOperatorText += expressionString.charAt(index);
            }
        }
    } while (appendOperation);

    return _logicOperator2.default.getLogicOperator(logicOperatorText.trim());
}

exports.default = { getLimit: getLimit, getOffset: getOffset, parseSeletor: parseSeletor, parseSortOrder: parseSortOrder, parseRequestSymbols: parseRequestSymbols, parseFilterExpressions: parseFilterExpressions };
module.exports = exports['default'];
//# sourceMappingURL=query.parser.js.map
