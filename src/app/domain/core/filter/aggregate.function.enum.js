const AggregateFunction = {
    'SUM': {
        'name': 'SUM',
        'sqlFunction': 'sum',
        'sqlFunctionTemplate': 'sum( ${ columnField } )'
    },
    'AVG': {
        'name': 'AVG',
        'sqlFunction': 'avg',
        'sqlFunctionTemplate': 'avg( ${ columnField } )'
    },
    'COUNT': {
        'name': 'COUNT',
        'sqlFunction': 'count',
        'sqlFunctionTemplate': 'count( ${ columnField } )'
    },
    'COUNT_DISTINCT': {
        'name': 'COUNT_DISTINCT',
        'sqlFunction': 'countDistinct',
        'sqlFunctionTemplate': 'count(distinct( ${ columnField } ))'
    },
    'GROUP_BY': {
        'name': 'GROUP_BY',
        'sqlFunction': 'groupBy',
        'sqlFunctionTemplate': 'groupBy'
    }
}

AggregateFunction.isAggregationFunction = functionName => {
    return functionName === AggregateFunction.SUM.sqlFunction || functionName === AggregateFunction.AVG.sqlFunction 
        || functionName === AggregateFunction.COUNT.sqlFunction;
};

export default AggregateFunction;