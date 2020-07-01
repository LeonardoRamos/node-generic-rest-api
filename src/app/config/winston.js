import winston from 'winston';
import config from './config';

let loggerConfig = {
    format: winston.format.combine(
        winston.format.timestamp(),
        winston.format.errors({ stack: true }),
        winston.format.simple(),
        winston.format.prettyPrint(),
        winston.format.colorize({ all: true })
    ),
    transports: [
        new winston.transports.Console({ 
            handleExceptions: true 
        }),
    ]
};

if (config.env === config.environments.production) {
    loggerConfig.transports.push(
        new winston.transports.File({ 
            handleExceptions: true,
            filename: config.logFile
        })
    );
}

const logger = winston.createLogger(loggerConfig);

export default logger;
