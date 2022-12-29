import LogicOperator from '../../domain/core/filter/logic.operator.enum';
import FilterOperator from '../../domain/core/filter/filter.operator.enum';
import SortOrder from '../../domain/core/filter/sort.order.enum';

const DEFAULT_OFFSET = 0;
const DEFAULT_LIMIT = 20;
const MAX_LIMIT = 100;

function hasValidAggregateFunction(requestQuery) {
    return (requestQuery.sum !== null && requestQuery.sum !== undefined && '' !== requestQuery.sum) || 
            (requestQuery.avg !== null && requestQuery.avg !== undefined && '' !== requestQuery.avg) || 
            (requestQuery.count !== null && requestQuery.count !== undefined && '' !== requestQuery.count) ||
            (requestQuery.countDistinct !== null && requestQuery.countDistinct !== undefined && '' !== requestQuery.countDistinct);
}

function getOffset(requestQuery) {
    if (requestQuery.offset === undefined || requestQuery.offset === null) {
        return DEFAULT_OFFSET;
    }

    return requestQuery.offset;
}

function getLimit(requestQuery) {
    let limit = requestQuery.limit;
    if (limit === undefined || limit === null) {
        limit = DEFAULT_LIMIT;
    
    } else if (limit > MAX_LIMIT) {
        limit = MAX_LIMIT;
    }

    return limit
}

function parseSeletor(seletor) {
    if (seletor === null || seletor === undefined || '' === seletor) {
        return [];
    }

    return seletor.split(',').map(attr => attr.trim());
}

function parseSortOrder(sort) {
    if (sort === null || sort === undefined || '' === sort) {
        return [];
    }

    let orderFields = [];
    let fields = sort.split(',').map(attr => attr.trim());

    for (const field of fields) {
        let filterOrder = field.split('=');
        orderFields.push({
            field: filterOrder[0].trim(),
            sortOrder: SortOrder.getSortOrder(filterOrder[1].trim())
        });
    }

    return orderFields;
}

function parseSymbols(requestQuery) {
    if (requestQuery.filter) {
        requestQuery.filter = requestQuery.filter.split('[').join('').split(']').join('');
        requestQuery.filter = requestQuery.filter.split(LogicOperator.AND.operatorAlias).join(LogicOperator.AND.operator);
        requestQuery.filter = requestQuery.filter.split(LogicOperator.OR.operatorAlias).join(LogicOperator.OR.operator);
        requestQuery.filter = parseFilterOperators(requestQuery.filter);
    }

    requestQuery = normalizeSymbol(requestQuery, 'projection');
    requestQuery = normalizeSymbol(requestQuery, 'sum');
    requestQuery = normalizeSymbol(requestQuery, 'avg');
    requestQuery = normalizeSymbol(requestQuery, 'count');
    requestQuery = normalizeSymbol(requestQuery, 'countDistinct');
    requestQuery = normalizeSymbol(requestQuery, 'groupBy');
    requestQuery = normalizeSymbol(requestQuery, 'sort');

    return requestQuery;
}

function normalizeSymbol(requestQuery, symbol) {
    if (requestQuery[symbol]) {
        requestQuery[symbol] = requestQuery[symbol].split('[').join('').split(']').join('');
    }
    
    return requestQuery;
}

function parseFilterOperators(filter) {
    let simpleCharOperator = [ FilterOperator.EQ, FilterOperator.GT, FilterOperator.LT ];
    
    Object.keys(FilterOperator).forEach(filterOperator => {
        if (!simpleCharOperator.includes(FilterOperator[filterOperator])) {
            filter = filter.split(FilterOperator[filterOperator].operatorCommonAlias).join(FilterOperator[filterOperator].parseableOperator);
            filter = filter.split(FilterOperator[filterOperator].operatorAlias).join(FilterOperator[filterOperator].parseableOperator);
        }
    });

    simpleCharOperator.forEach(simpleCharfilterOperator => {
        filter = filter.split(simpleCharfilterOperator.operatorCommonAlias).join(simpleCharfilterOperator.parseableOperator);
        filter = filter.split(simpleCharfilterOperator.operatorAlias).join(simpleCharfilterOperator.parseableOperator);
    });

    return filter;
}

function parseFilterExpressions(expressionString) {
    let currentExpression = {};
    
    if (expressionString === null) {
        return currentExpression;
    }
    
    let initialExpression = currentExpression;

    if (expressionString) {
        let word = '';
        let i = 0;

        while (i < expressionString.length) {
        
            if (expressionString.charAt(i) !== '_') {
                word += expressionString.charAt(i);
                
            } else {
                let logicOperator = processOperator(expressionString, i);
    
                if (logicOperator !== null) {
                    currentExpression.logicOperator = logicOperator;
                    currentExpression.filterField = parseFilterField(word.trim());
                    currentExpression = processNewExpressionNode(currentExpression);
                    
                    i += logicOperator.operator.length - 1;
                    word = '';
                
                } else {
                    word += expressionString.charAt(i);
                }
            }
    
            i++;
        }
    }
    
    currentExpression.filterField = parseFilterField(word.trim());
    processNewExpressionNode(currentExpression);
    
    return initialExpression;
}

function parseFilterField(logicExpression) {
    let filterField = {};
    
    if (logicExpression === null || '' === logicExpression) {
        return null;
    }
    
    let word = '';
    let i = 0;

    while (i < logicExpression.length) {
      
        if (logicExpression.charAt(i) !== '|') {
            word += logicExpression.charAt(i);
        
        } else {
            let filterOperator = getComparisonOperator(logicExpression, i);
            
            filterField.field = word.trim();
            filterField.filterOperator = filterOperator;
            
            i += filterOperator.parseableOperator.length - 1;
            word = '';
        }

        i++;
    }
    
    filterField.value = word.trim();
    
    return filterField;
}

function getComparisonOperator(logicExpression, index) {
    let operation = '';
    let appendOperation = true; 
    
    do {
        operation += logicExpression.charAt(index);
        index++;
        
        if (index >= logicExpression.length || logicExpression.charAt(index) === '|') {
            
            appendOperation = false;
            
            if (index < logicExpression.length && logicExpression.charAt(index) === '|') {
                operation += logicExpression.charAt(index);
            }
        }
        
    } while (appendOperation);
    
    return FilterOperator.getFilterOperator(operation.trim());
}

function processNewExpressionNode(currentExpression) {
    if (currentExpression.logicOperator !== null) {
        currentExpression.filterExpression = {};
        currentExpression = currentExpression.filterExpression;
    }
    
    return currentExpression;
}

function processOperator(expressionString, index) {
    let logicOperatorText = '';
    let appendOperation = true;
    
    do {
        logicOperatorText += expressionString.charAt(index);
        index++;
        
        if (index >= expressionString.length || expressionString.charAt(index) === '_') {
            
            appendOperation = false;
            
            if (index < expressionString.length && expressionString.charAt(index) === '_') {
                logicOperatorText += expressionString.charAt(index);
            }
        }
        
    } while (appendOperation);
    
    return LogicOperator.getLogicOperator(logicOperatorText.trim());
}

export default { 
    hasValidAggregateFunction, 
    getLimit, 
    getOffset, 
    parseSeletor, 
    parseSortOrder, 
    parseSymbols, 
    parseFilterExpressions 
};
