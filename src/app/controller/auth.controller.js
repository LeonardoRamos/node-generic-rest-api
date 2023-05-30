import jwt from 'jsonwebtoken';
import bcrypt from 'bcrypt';
import { Joi } from 'express-validation';
import httpStatus from 'http-status';
import { ApiError, config } from 'generic-rest-express-lib';
import UserService from '../service/user.service';

const userService = new UserService();

function login(req, res, next) {
    const user = userService.findByEmail(req.body.email);

    if (user && bcrypt.compareSync(req.body.password, user.password)) {
        const token = jwt.sign({
            externalId: user.externalId,
            name: user.name,
            email: user.email,
            role: user.role,
            expiresIn: config.jwtExpiration,
        }, config.jwtSecret);

        return res.json({ token });
    }

    const err = new ApiError(
        'Wrong username or password',
        'AUTHENTICATION_ERROR',
        httpStatus.UNAUTHORIZED,
        true
    );

    return next(err);
}

function paramValidation() {
    return {
        login: {
            body: Joi.object({
                email: Joi.string().required(),
                password: Joi.string().required(),
            }),
        },
    };
}

export default { login, paramValidation };