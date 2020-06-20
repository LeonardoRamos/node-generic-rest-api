import winston from 'winston';

const logger = winston.createLogger({
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
});

export default logger;
