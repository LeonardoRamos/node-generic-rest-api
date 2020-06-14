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

        removeField(row);

        return row;
    });
}

function formatAggregateFields(row, aggregation, sqlFunction) {
    for (let i = 0; i < aggregation[sqlFunction].length; i++) {
        let splittedField = aggregation[sqlFunction][i].split('.');

        let rootField = splittedField[0];
        let rowField = row[rootField];

        let matchField = true;

        for (let j = 1; j < splittedField.length; j++) {
            let subField = splittedField[j];

            if (rowField[subField]) {
                rowField = rowField[subField];
            } else {
                matchField = false;
                break;
            }
        }

        if (typeof rowField === 'object') {
            matchField = false;
        }

        row[sqlFunction] = row[sqlFunction] || {};
        row[sqlFunction][rootField] = row[rootField];

        delete row[rootField];
    }
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
        
        if (!isAggregationField(field)) {
            maskNestedObject(row[field]);
        }
    });
}

function isAggregationField(field) {
    return field === AggregateFunction.SUM.function || field === AggregateFunction.AVG.function 
            || field === AggregateFunction.COUNT.function;
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

export default { mapResulRecords, mapResultMetadata };