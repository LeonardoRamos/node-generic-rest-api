const FilterOperator = {
    'EQ': {
        'name': 'EQ', 
        'operatorAlias': '=eq=',
        'operatorCommonAlias': '=',
        'parseableOperator': '|eq|'
    },
    'LE': {
        'name': 'LE', 
        'operatorAlias': '=le=',
        'operatorCommonAlias': '<=',
        'parseableOperator': '|le|'
    },
    'GE': {
        'name': 'GE', 
        'operatorAlias': '=ge=',
        'operatorCommonAlias': '>=',
        'parseableOperator': '|ge|'
    },
    'GT': {
        'name': 'GT', 
        'operatorAlias': '=gt=',
        'operatorCommonAlias': '>',
        'parseableOperator': '|gt|'
    },
    'LT': {
        'name': 'LT', 
        'operatorAlias': '=lt=',
        'operatorCommonAlias': '<',
        'parseableOperator': '|lt|'
    },
    'NE': {
        'name': 'NE', 
        'operatorAlias': '=ne=',
        'operatorCommonAlias': '!=',
        'parseableOperator': '|ne|'
    },
    'IN': {
        'name': 'IN', 
        'operatorAlias': '=in=',
        'operatorCommonAlias': '=in=',
        'parseableOperator': '|in|'
    },
    'OU': {
        'name': 'OU', 
        'operatorAlias': '=out=',
        'operatorCommonAlias': '=out=',
        'parseableOperator': '|ou|'
    },
    'LK': {
        'name': 'LK', 
        'operatorAlias': '=like=',
        'operatorCommonAlias': '=like=',
        'parseableOperator': '|lk|'
    }
}

FilterOperator.getFilterOperator = operator => {
    let filterOperators = Object.keys(FilterOperator);
    
    for (const op of filterOperators) {
        let filterOperator = FilterOperator[op];

        if (filterOperator.name.toUpperCase() === operator.toUpperCase() 
            || filterOperator.operatorAlias.toUpperCase() === operator.toUpperCase()
            || filterOperator.operatorCommonAlias.toUpperCase() === operator.toUpperCase()
            || filterOperator.parseableOperator.toUpperCase() === operator.toUpperCase()) {
            
            return filterOperator;
        }
    }
    
    return null;
}

export default FilterOperator;