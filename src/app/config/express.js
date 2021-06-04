import express from 'express';
import logger from 'morgan';
import cookieParser from 'cookie-parser';
import compress from 'compression';
import methodOverride from 'method-override';
import cors from 'cors';
import httpStatus from 'http-status';
import expressWinston from 'express-winston';
import expressValidation from 'express-validation';
import helmet from 'helmet';
import winstonInstance from './winston';
import authRoute from '../route/auth.route';
import manageRoutes from '../route/base/manage.route';
import apiRoutes from '../route/api/api.route';
import config from './config';
import ApiError from '../error/api.error';

const app = express();

if (config.env === config.environments.development) {
    app.use(logger('dev'));
}

app.use(express.json());
app.use(express.urlencoded({ extended: true }));
app.use(cookieParser());
app.use(compress());
app.use(methodOverride());
app.use(helmet());
app.use(cors());

if (config.env === config.environments.development) {
    expressWinston.requestWhitelist.push('body');
    expressWinston.responseWhitelist.push('body');
    app.use(expressWinston.logger({
        winstonInstance,
        meta: true, 
        msg: 'HTTP {{req.method}} {{req.url}} {{res.statusCode}} {{res.responseTime}}ms',
        colorize: true
    }));
}

app.use('/auth', authRoute);
app.use('/manage', manageRoutes);
app.use('/v1', apiRoutes);

app.use((err, req, res, next) => {
    if (!(err instanceof ApiError)) {
        winstonInstance.error(err);
        
        let code = 'ERROR_CODE';
        
        if (err.name === 'UnauthorizedError') {
            code = 'AUTH_ERROR_' + err.code.toUpperCase();
        
        } else if (err.code) {
            code = err.code.toUpperCase();
        }

        const apiError = new ApiError(err.message, code, err.status, err.isPublic);
        return next(apiError);
    }
    
    return next(err);
});

app.use((req, res, next) => {
    const err = new ApiError('API not found', 'API_NOT_FOUND', httpStatus.NOT_FOUND);
    return next(err);
});

if (config.env !== config.environments.test) {
    app.use(expressWinston.errorLogger({
        winstonInstance,
    }));
}

app.use(( err, req, res, next, ) => {
    if (err instanceof expressValidation.ValidationError) {
        const errors = err.details.body
            .map(error => {
                return { 
                    code: 'VALIDATION_ERROR_' + error.context.key.toUpperCase(), 
                    message: error.message,
                    stack: config.env === config.environments.development ? error.type : {}
                } 
            });

        res.status(err.status).json({ errors });

    } else {
        res.status(err.status).json({
            errors: [{
                code: err.code,
                message: err.isPublic ? err.message : httpStatus[err.status],
                stack: config.env === config.environments.development ? err.stack : {}
            }]
        });
    } 
});

export default app;
