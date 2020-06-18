import Joi from '@hapi/joi';

require('dotenv').config();

const envVarsSchema = Joi.object({
    NODE_ENV: Joi.any()
        .valid(...['development', 'production', 'test', 'provision'])
        .default('development'),

    PORT: Joi.number().default(9503),

    JWT_EXPIRATION: Joi.number().default(86000),
    JWT_SECRET: Joi.string()
        .description('JWT Secret required to sign')
        .default('6cfaef779158723ca4998ec416d18eb8'),

    PG_DB: Joi.string()
        .description('Postgres database name')
        .default('TestNode'),

    PG_PORT: Joi.number().default(5432),
    PG_HOST: Joi.string().default('localhost'),
    PG_USER: Joi.string()
        .description('Postgres username')
        .default('postgres'),

    PG_PASSWORD: Joi.string()
        .allow('')
        .description('Postgres password')
        .default('postgres')
        .optional(),

})
    .unknown()
    .required();

const { error, value: envVars } = envVarsSchema.validate(process.env);
if (error) {
    throw new Error(`Config validation error: ${error.message}`);
}

const config = {
    env: envVars.NODE_ENV,
    port: envVars.PORT,
    jwtSecret: envVars.JWT_SECRET,
    jwtExpiration: envVars.JWT_EXPIRATION,
    postgres: {
        db: envVars.PG_DB,
        port: envVars.PG_PORT,
        host: envVars.PG_HOST,
        user: envVars.PG_USER,
        password: envVars.PG_PASSWORD,
    },
};

export default config;
