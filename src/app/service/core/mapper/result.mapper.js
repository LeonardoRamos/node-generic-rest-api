import queryParser from '../query.parser';
import AggregateFunction from '../../../domain/core/filter/aggregate.function.enum';

function mapResulRecords(result, requestQuery) {
    let aggregation = {
        sum: queryParser.parseSeletor(requestQuery.sum),
        avg: queryParser.parseSeletor(requestQuery.avg),
        count: queryParser.parseSeletor(requestQuery.count)
            .concat(queryParser.parseSeletor(requestQuery.countDistinct))
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
    for (let i = 0; i < aggregation[sqlFunction].length; i++) {
        let splittedField = aggregation[sqlFunction][i].split('.');

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

        if (queryParser.hasValidAggregateFunction(requestQuery)) {
            metadata.pageSize = metadata.totalCount;
        } else {
            metadata.pageSize = query.limit;
        }

        return metadata;
    }

    metadata.totalCount = mapGroupCount(result);

    if (queryParser.hasValidAggregateFunction(requestQuery)) {
        metadata.pageSize = metadata.totalCount;
    } else {
        metadata.pageSize = query.limit;
    }

    return metadata;
}

function mapGroupCount(result) {
    let count = 0;
    
    for (let i = 0; i < result.count.length; i++) {
        count += +result.count[i].count;
    }

    return count;
}

export default { mapResulRecords, mapResultMetadata };
