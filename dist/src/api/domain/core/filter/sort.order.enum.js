'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});
var SortOrder = {
    'ASC': {
        'name': 'ASC',
        'order': 'asc'
    },
    'DESC': {
        'name': 'DESC',
        'order': 'desc'
    }
};

SortOrder.getSortOrder = function (order) {
    var sortOrders = Object.keys(SortOrder);

    for (var i = 0; i < sortOrders.length; i++) {
        var sortOrder = SortOrder[sortOrders[i]];

        if (sortOrder.name.toUpperCase() === order.toUpperCase() || sortOrder.order.toUpperCase() === order.toUpperCase()) {

            return sortOrder;
        }
    }

    return null;
};

exports.default = SortOrder;
module.exports = exports['default'];
//# sourceMappingURL=sort.order.enum.js.map
