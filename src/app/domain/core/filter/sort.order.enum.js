const SortOrder = {
    'ASC': {
        'name': 'ASC',
        'order': 'asc'
    },
    'DESC': {
        'name': 'DESC',
        'order': 'desc'
    }
}

SortOrder.getSortOrder = order => {
    let sortOrders = Object.keys(SortOrder);
    
    for (const so of sortOrders) {
        let sortOrder = SortOrder[so];

        if (sortOrder.name.toUpperCase() === order.toUpperCase() 
            || sortOrder.order.toUpperCase() === order.toUpperCase()) {
            
            return sortOrder;
        }
    }

    return null;
}

export default SortOrder;