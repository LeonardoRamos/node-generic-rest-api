import 'babel-polyfill';
import config from './src/api/config/config';
import app from './src/api/config/express';
import db from './src/api/config/sequelize';

const debug = require('debug')('express-postgresql-sequelize-es6-rest-api:index');

Promise = require('bluebird'); 

if (!module.parent) {
    app.listen(config.port, () => {
        console.info(`server started on port ${config.port} (${config.env})`);
    });
}

export default app;
