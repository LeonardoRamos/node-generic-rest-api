import { v4 as uuidv4 } from 'uuid';
import Sequelize from 'sequelize';
import queryBuilder from '../../helpers/query.builder';
import resultMapper from '../../helpers/result.mapper';

module.exports = class ApiRestService {
    
    constructor(model) {
        this.model = model;

        this.findBySlug = this.findBySlug.bind(this);
        this.save = this.save.bind(this);
        this.findAll = this.findAll.bind(this);
    }

    async findBySlug(slug) {
        return await this.model.findOne({ 
            where: { 
                slug 
            } 
        });   
    }

    async findAll(requestQuery) {
        let query = queryBuilder.buildQuery(this.model, requestQuery);
        let result = await this.model.findAndCountAll(query);

        return {
            records: resultMapper.mapResulRecords(result, requestQuery),
            metadata: resultMapper.mapResultMetadata(query, result)
        };
    }

    async save(entity) {
        const entityModel = this.model.build(entity);
        
        entityModel.slug = uuidv4().split('-').join('').toUpperCase();
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
