import requestParser from '../request.parser';
import AggregateFunction from '../../../domain/core/filter/aggregate.function.enum';

function mapResulRecords(result, requestQuery) {
    let aggregation = {
        sum: requestParser.parseSeletor(requestQuery.sum),
        avg: requestParser.parseSeletor(requestQuery.avg),
        count: requestParser.parseSeletor(requestQuery.count)
            .concat(requestParser.parseSeletor(requestQuery.countDistinct))
    };

    return result.rows.map((row) => {
        formatAggregateFields(row, aggregation, AggregateFunction.SUM.sqlFunction);
        formatAggregateFields(row, aggregation, AggregateFunction.COUNT.sqlFunction);
        formatAggregateFields(row, aggregation, AggregateFunction.AVG.sqlFunction);

        hideFields(row);

        return row;
    });
}

function formatAggregateFields(row, aggregation, sqlFunction) {
    for (const agreggationFunction of aggregation[sqlFunction]) {
        let splittedField = agreggationFunction.split('.');

        let rootField = splittedField[0];

        row[sqlFunction] = row[sqlFunction] || {};

        if (row[sqlFunction][rootField] !== undefined) {
            row[sqlFunction][rootField] = { ...row[sqlFunction][rootField], ...row[rootField] };
        
        } else {
            row[sqlFunction][rootField] = row[rootField];
        }

        delete row[rootField];
    }
}

function hideFields(row) {
    Object.keys(row).forEach((field) => {
        if (field === 'id' || field.startsWith('id_') || row[field] === null) {
            delete row[field];
        
        } else if (!AggregateFunction.isAggregationFunction(field)) {
            let fieldValue = row[field];
            
            if (typeof fieldValue === 'object') {
                hideFields(fieldValue);
        
            } else if (Array.isArray(fieldValue)) {
                fieldValue.forEach((nestedRow) => {
                    hideFields(nestedRow);
                });
            }
        }
    });
}

function mapResultMetadata(query, result, requestQuery) {
    let metadata = {
        pageOffset: query.offset
    };

    if (!query.group) {
        metadata.totalCount = +result.count;

        if (requestParser.hasValidAggregateFunction(requestQuery)) {
            metadata.pageSize = metadata.totalCount;
        } else {
            metadata.pageSize = query.limit;
        }

        return metadata;
    }

    metadata.totalCount = mapGroupCount(result);

    if (requestParser.hasValidAggregateFunction(requestQuery)) {
        metadata.pageSize = metadata.totalCount;
    } else {
        metadata.pageSize = query.limit;
    }

    return metadata;
}

function mapGroupCount(result) {
    let count = 0;

    for (const resultCount of result.count) {
        count += +resultCount.count;
    }

    return count;
}

export default { mapResulRecords, mapResultMetadata };
