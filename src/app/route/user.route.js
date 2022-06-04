import config from '../config/config';
import express from 'express';
import expressJwt from 'express-jwt';
import { validate } from 'express-validation';
import UserController from '../controller/user.controller';

const router = express.Router();
const userController = new UserController();
const paramValidation = userController.paramValidation();

router
    .route('/')
    .get(
        expressJwt({ secret: config.jwtSecret }),
        userController.list)
    .post([ 
            expressJwt({ secret: config.jwtSecret }), 
            validate(paramValidation.insert) 
        ], 
        userController.insert);

router
    .route('/:externalId')
    .get(
        expressJwt({ secret: config.jwtSecret }),
        userController.get)
    .put([
            expressJwt({ secret: config.jwtSecret }), 
            validate(paramValidation.update) 
        ], 
        userController.update)
    .delete([
            expressJwt({ secret: config.jwtSecret }),
            validate(paramValidation.remove)
        ], 
        userController.remove);

router.param('externalId', userController.getByExternalId);

export default router;
