const AggregateFunction = {
    'SUM': {
        'name': 'SUM',
        'function': 'sum'
    },
    'AVG': {
        'name': 'AVG',
        'function': 'avg'
    },
    'COUNT': {
        'name': 'COUNT',
        'function': 'count'
    },
    'COUNT_DISTINCT': {
        'name': 'COUNT_DISTINCT',
        'function': 'count(distinct'
    },
    'GROUP_BY': {
        'name': 'GROUP_BY',
        'function': 'groupBy'
    }
}

AggregateFunction.isAggregationFunction = functionName => {
    return functionName === AggregateFunction.SUM.function || functionName === AggregateFunction.AVG.function 
        || functionName === AggregateFunction.COUNT.function;
};

export default AggregateFunction;