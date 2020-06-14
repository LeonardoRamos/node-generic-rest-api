import 'babel-polyfill';
import config from './src/app/config/config';
import app from './src/app/config/express';
import db from './src/app/config/sequelize';

const debug = require('debug')('node-generic-rest-api:app');

Promise = require('bluebird'); 

if (!module.parent) {
    app.listen(config.port, () => {
        console.info(`server started on port ${config.port} (${config.env})`);
    });
}

export default app;
