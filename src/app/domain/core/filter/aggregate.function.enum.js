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

export default AggregateFunction;