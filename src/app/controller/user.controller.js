import { Joi } from 'express-validation';
import { ApiRestController } from 'generic-rest-express-lib';
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
                    body: Joi.object({
                        username: Joi.string().required(),
                    })
                },
                update: {
                    body: Joi.object({
                        username: Joi.string().required(),
                    })
                }
            }
        }
    }

}
