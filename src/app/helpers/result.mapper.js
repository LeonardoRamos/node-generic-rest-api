import queryParser from './query.parser';

function mapResulRecords(result) {
    return result.rows.map((row) => {
        removeField(row);
        return row;
    });
}

function maskNestedObject(fieldValue) {
    if (fieldValue !== null && typeof fieldValue === 'object') {
        removeField(fieldValue)

    } else if (Array.isArray(fieldValue)) {
        fieldValue.forEach((row) => {
            removeField(row);
        })
    }
}

function removeField(row) {
    let fields = Object.keys(row).forEach((field) => {
        if (field === 'id' || field.startsWith('id_') || row[field] === null) {
            delete row[field];
        }
        
        maskNestedObject(row[field]);
    });
}

function mapResultMetadata(query, result) {
    let metadata = {
        pageOffset: query.offset,
        pageSize: query.limit
    };

    if (!query.group) {
        metadata.count = +result.count;
        return metadata;
    }
    
    let count = 0;

    for (let i = 0; i < result.count.length; i++) {
        count += +result.count[i].count;
    }

    metadata.count = +count;

    return metadata;
}

export default { 
    mapResulRecords, 
    mapResultMetadata 
};