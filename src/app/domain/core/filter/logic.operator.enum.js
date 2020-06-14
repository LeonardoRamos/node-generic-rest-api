const LogicOperator = {
    'OR': {
        'name': 'OR',
        'operator': '_or_',
        'operatorAlias': ','
    },
    'AND': {
        'name': 'AND',
        'operator': '_and_',
        'operatorAlias': ';'
    }
}

LogicOperator.isOrOperator = logicOperator => {
    return LogicOperator.OR.operator.toUpperCase() === logicOperator.toUpperCase() 
        || LogicOperator.OR.operatorAlias.toUpperCase() === logicOperator.toUpperCase() 
        || LogicOperator.OR.name.toUpperCase() === logicOperator.toUpperCase();
};

LogicOperator.isAndOperator = logicOperator => {
    return LogicOperator.AND.operator.toUpperCase() === logicOperator.toUpperCase() 
        || LogicOperator.AND.operatorAlias.toUpperCase() === logicOperator.toUpperCase() 
        || LogicOperator.AND.name.toUpperCase() === logicOperator.toUpperCase();
};

LogicOperator.getLogicOperator = logicOperator => {
    if (LogicOperator.isOrOperator(logicOperator)) {
        return LogicOperator.OR;
    
    } else if (LogicOperator.isAndOperator(logicOperator)) {
        return LogicOperator.AND;
    }
    
    return null;
};

export default LogicOperator;