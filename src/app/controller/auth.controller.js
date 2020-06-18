import jwt from 'jsonwebtoken';
import bcrypt from 'bcrypt';
import { Joi } from 'express-validation';
import httpStatus from 'http-status';
import ApiError from '../helpers/api.error';
import config from '../config/config';
import UserService from '../service/user.service';

const userService = new UserService();

async function login(req, res, next) {
    const user = await userService.findByEmail(req.body.email);

    if (user && bcrypt.compareSync(req.body.password, user.password)) {
        const token = jwt.sign({
            slug: user.slug,
            name: user.name,
            email: user.email,
            role: user.role,
            expiresIn: config.jwtExpiration,
        }, config.jwtSecret);

        return res.json({ token });
    }

    const err = new ApiError('Authentication error', httpStatus.UNAUTHORIZED, true);
 
    return next(err);
}

function paramValidation() {
    return { 
        login: {
            body: Joi.object({
                email: Joi.string().required(),
                password: Joi.string().required(),
            }),
        }
    }
}

export default { login, paramValidation };
