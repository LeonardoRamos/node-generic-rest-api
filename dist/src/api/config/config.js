'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _joi = require('joi');

var _joi2 = _interopRequireDefault(_joi);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

require('dotenv').config();

var envVarsSchema = _joi2.default.object({
    NODE_ENV: _joi2.default.string().allow(['development', 'production', 'test', 'provision']).default('development'),

    PORT: _joi2.default.number().default(9503),

    JWT_EXPIRATION: _joi2.default.number().default(86000000),
    JWT_SECRET: _joi2.default.string().description('JWT Secret required to sign').default('6cfaef779158723ca4998ec416d18eb8'),

    PG_DB: _joi2.default.string().description('Postgres database name').default('Test'),

    PG_PORT: _joi2.default.number().default(5432),
    PG_HOST: _joi2.default.string().default('localhost'),
    PG_USER: _joi2.default.string().description('Postgres username').default('postgres'),

    PG_PASSWORD: _joi2.default.string().allow('').description('Postgres password').default('postgres')

}).unknown().required();

var _Joi$validate = _joi2.default.validate(process.env, envVarsSchema),
    error = _Joi$validate.error,
    envVars = _Joi$validate.value;

if (error) {
    throw new Error('Config validation error: ' + error.message);
}

var config = {
    env: envVars.NODE_ENV,
    port: envVars.PORT,
    jwtSecret: envVars.JWT_SECRET,
    jwtExpiration: envVars.JWT_EXPIRATION,
    postgres: {
        db: envVars.PG_DB,
        port: envVars.PG_PORT,
        host: envVars.PG_HOST,
        user: envVars.PG_USER,
        password: envVars.PG_PASSWORD
    }
};

exports.default = config;
module.exports = exports['default'];
//# sourceMappingURL=config.js.map
