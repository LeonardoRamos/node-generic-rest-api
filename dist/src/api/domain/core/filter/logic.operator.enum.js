'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
var LogicOperator = {
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
};

LogicOperator.isOrOperator = function (logicOperator) {
    return LogicOperator.OR.operator.toUpperCase() === logicOperator.toUpperCase() || LogicOperator.OR.operatorAlias.toUpperCase() === logicOperator.toUpperCase() || LogicOperator.OR.name.toUpperCase() === logicOperator.toUpperCase();
};

LogicOperator.isAndOperator = function (logicOperator) {
    return LogicOperator.AND.operator.toUpperCase() === logicOperator.toUpperCase() || LogicOperator.AND.operatorAlias.toUpperCase() === logicOperator.toUpperCase() || LogicOperator.AND.name.toUpperCase() === logicOperator.toUpperCase();
};

LogicOperator.getLogicOperator = function (logicOperator) {
    if (LogicOperator.isOrOperator(logicalOperator)) {
        return LogicOperator.OR;
    } else if (LogicOperator.isAndOperator(logicalOperator)) {
        return LogicOperator.AND;
    }

    return null;
};

exports.default = LogicOperator;
module.exports = exports['default'];
//# sourceMappingURL=logic.operator.enum.js.map
