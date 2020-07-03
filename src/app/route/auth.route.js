import express from 'express';
import { validate } from 'express-validation';
import expressJwt from 'express-jwt';
import authController from '../controller/auth.controller';

const router = express.Router();
const paramValidation = authController.paramValidation();

router
    .route('/login')
    .post(
        validate(paramValidation.login), 
        authController.login);

export default router;
