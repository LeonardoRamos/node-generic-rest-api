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
    if (err instanceof expressValidation.ValidationError) {
        const unifiedErrorMessage = err.errors
            .map(error => error.messages.join('. '))
            .join(' and ');
        const error = new ApiError(unifiedErrorMessage, err.status, true);
    
        return next(error);
    
    } else if (!(err instanceof ApiError)) {
        winstonInstance.error(err);
        const apiError = new ApiError(err.message, err.status, err.isPublic);
        return next(apiError);
    }
    
    return next(err);
});

app.use((req, res, next) => {
    const err = new ApiError('API not found', httpStatus.NOT_FOUND);
    return next(err);
});

if (config.env !== config.environments.test) {
    app.use(expressWinston.errorLogger({
        winstonInstance,
    }));
}

app.use(( err, req, res, next, ) =>
    res.status(err.status).json({
        message: err.isPublic ? err.message : httpStatus[err.status],
        stack: config.env === config.environments.development ? err.stack : {},
    }));

export default app;
