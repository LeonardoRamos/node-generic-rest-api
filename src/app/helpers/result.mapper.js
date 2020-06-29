import queryParser from './query.parser';
import AggregateFunction from '../domain/core/filter/aggregate.function.enum';

function mapResulRecords(result, requestQuery) {
    return result.rows.map((row) => {
        let aggregation = {
            sum: queryParser.parseSeletor(requestQuery.sum),
            avg: queryParser.parseSeletor(requestQuery.avg),
            count: queryParser.parseSeletor(requestQuery.count)
                .concat(queryParser.parseSeletor(requestQuery.countDistinct))
        };

        formatAggregateFields(row, aggregation, AggregateFunction.SUM.function);
        formatAggregateFields(row, aggregation, AggregateFunction.COUNT.function);
        formatAggregateFields(row, aggregation, AggregateFunction.AVG.function);

        hideFields(row);

        return row;
    });
}

function formatAggregateFields(row, aggregation, sqlFunction) {
    for (let i = 0; i < aggregation[sqlFunction].length; i++) {
        let splittedField = aggregation[sqlFunction][i].split('.');

        let rootField = splittedField[0];

        row[sqlFunction] = row[sqlFunction] || {};

        if (row[sqlFunction][rootField]) {
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
        
        } else if (!isAggregationField(field)) {
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

function isAggregationField(field) {
    return field === AggregateFunction.SUM.function || field === AggregateFunction.AVG.function 
            || field === AggregateFunction.COUNT.function;
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
    
    let count = 0;

    for (let i = 0; i < result.count.length; i++) {
        count += +result.count[i].count;
    }

    metadata.totalCount = count;

    if (queryParser.hasValidAggregateFunction(requestQuery)) {
        metadata.pageSize = metadata.totalCount;
    } else {
        metadata.pageSize = query.limit;
    }

    return metadata;
}

export default { mapResulRecords, mapResultMetadata };
