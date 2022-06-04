import { Joi } from 'express-validation';
import httpStatus from 'http-status';
import Sequelize from 'sequelize';
import ApiError from '../../error/api.error';

module.exports = class ApiRestController {
    
    constructor(apiService) {
        this.apiService = apiService;

        this.getByExternalId = this.getByExternalId.bind(this);
        this.insert = this.insert.bind(this);
        this.update = this.update.bind(this);
        this.list = this.list.bind(this);
        this.remove = this.remove.bind(this);
    }

    async getByExternalId(req, res, next, externalId) {
        try {
            const entityFound = await this.apiService.findByExternalId(externalId);
            
            if (!entityFound) {
                const e = new ApiError(
                    'No entity found for externalId [' + externalId + ']', 
                    'ENTITY_NOT_FOUND_ERROR',
                    httpStatus.NOT_FOUND,
                    true
                );
                    
                return next(e);
            }
            
            req.entity = entityFound;
            
            return next();
    
        } catch (error) {
            return this.handleError(error, next);
        }
    }
    
    get(req, res) {
        return res.json(req.entity);
    }
    
    insert(req, res, next) {
        return this.apiService.save(req.body)
            .then(savedEntity => res.json(savedEntity))
            .catch(e => {
                return this.handleError(e, next);
            });
    }
    
    update(req, res, next) {
        const entity = { ...req.entity, ...req.body };
    
        return this.apiService.update(entity)
            .then(savedEntity => res.json(savedEntity))
            .catch(e => {
                return this.handleError(e, next);
            });
    }
    
    list(req, res, next) {
        return this.apiService.findAll(req.query)
            .then(entities => res.json(entities))
            .catch(e => {
                return this.handleError(e, next);
            });
    }
    
    remove(req, res, next) {
        return this.apiService.delete(req.entity)
            .then(() => res.status(204))
            .catch(e => {
                return this.handleError(e, next);
            });
    }

    handleError(e, next) {
        if (e instanceof Sequelize.BaseError) {
            e.status = httpStatus.BAD_REQUEST;
        }

        return next(e);
    }

    paramValidation() {
        return {
            insert: {
                body: {}
            },
            update: {
                body: {},
                params: Joi.object({
                    externalId: Joi.string().hex().required(),
                })
            },
            remove: {
                params: Joi.object({
                    externalId: Joi.string().hex().required(),
                })
            }
        };
    }

}