import Joi from 'joi';
import ApiRestController from './core/api.rest.controller';
import UserService from '../service/user.service';

const userService = new UserService();

module.exports = class UserController extends ApiRestController {
    
    constructor() {
        super(userService);
    }

    paramValidation() {
        return { 
            ...super.paramValidation(), 
            ...{ 
                insert: {
                    body: {
                        username: Joi.string().required(),
                    }
                },
                update: {
                    body: {
                        username: Joi.string().required(),
                    }
                }
            }
        }
    }

}
