import { v4 as uuidv4 } from 'uuid';
import Sequelize from 'sequelize';
import queryBuilder from './query.builder';
import resultMapper from './mapper/result.mapper';

module.exports = class ApiRestService {
    
    constructor(model) {
        this.model = model;

        this.findByExternalId = this.findByExternalId.bind(this);
        this.save = this.save.bind(this);
        this.findAll = this.findAll.bind(this);
    }

    async findByExternalId(externalId) {
        return await this.model.findOne({ 
            where: { externalId } 
        });   
    }

    async findAll(requestQuery) {
        let query = queryBuilder.buildQuery(this.model, requestQuery);
        let result = await this.model.findAndCountAll(query);

        return {
            records: resultMapper.mapResulRecords(result, requestQuery),
            metadata: resultMapper.mapResultMetadata(query, result, requestQuery)
        };
    }

    async save(entity) {
        const entityModel = this.model.build(entity);
        
        entityModel.externalId = uuidv4().split('-').join('');
        entityModel.insertDate = Sequelize.NOW;
        entityModel.updateDate = entityModel.insertDate;

        return await entityModel.save();
    }

    async update(entityModel) {
        entityModel.updateDate = Sequelize.NOW;
        return await entityModel.save();
    }

    async delete(entityModel) {
        return await entityModel.destroy();
    }

}
