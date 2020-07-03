const AggregateFunction = {
    'SUM': {
        'name': 'SUM',
        'sqlFunction': 'sum'
    },
    'AVG': {
        'name': 'AVG',
        'sqlFunction': 'avg'
    },
    'COUNT': {
        'name': 'COUNT',
        'sqlFunction': 'count'
    },
    'COUNT_DISTINCT': {
        'name': 'COUNT_DISTINCT',
        'sqlFunction': 'count(distinct'
    },
    'GROUP_BY': {
        'name': 'GROUP_BY',
        'sqlFunction': 'groupBy'
    }
}

AggregateFunction.isAggregationFunction = functionName => {
    return functionName === AggregateFunction.SUM.sqlFunction || functionName === AggregateFunction.AVG.sqlFunction 
        || functionName === AggregateFunction.COUNT.sqlFunction;
};

export default AggregateFunction;