import Joi from 'joi';
import httpStatus from 'http-status';

module.exports = class ApiRestController {
    
    constructor(apiService) {
        this.apiService = apiService;

        this.getBySlug = this.getBySlug.bind(this);
        this.insert = this.insert.bind(this);
        this.update = this.update.bind(this);
        this.list = this.list.bind(this);
        this.remove = this.remove.bind(this);
    }

    async getBySlug(req, res, next, slug) {
        try {
            const entityFound = await this.apiService.findBySlug(slug);
            
            if (!entityFound) {
                const e = new Error('Entity not found');
                e.status = httpStatus.NOT_FOUND;
                return next(e);
            }
            
            req.entity = entityFound;
            
            return next();
    
        } catch (error) {
            return next(error);
        }
    }
    
    get(req, res) {
        return res.json(req.entity);
    }
    
    insert(req, res, next) {
        return this.apiService.save(req.body)
            .then(savedEntity => res.json(savedEntity))
            .catch(e => next(e));
    }
    
    update(req, res, next) {
        const entity = { ...req.entity, ...req.body };
    
        return this.apiService.update(entity)
            .then(savedEntity => res.json(savedEntity))
            .catch(e => next(e));
    }
    
    list(req, res, next) {
        return this.apiService.findAll(req)
            .then(entities => res.json(entities))
            .catch(e => next(e));
    }
    
    remove(req, res, next) {
        return this.apiService.delete(req.entity)
            .then(() => res.status(204))
            .catch(e => next(e));
    }

    paramValidation() {
        return {
            insert: {
                body: {}
            },
            update: {
                body: {},
                params: {
                    slug: Joi.string().hex().required(),
                }
            }
        };
    }

}