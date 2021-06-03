import '@babel/polyfill';
import config from './config/config';
import app from './config/express';

Promise = require('bluebird'); 

if (!module.children) {
    app.listen(config.port, () => {
        console.info(`server started on port ${config.port} (${config.env})`);
    });
}

export default app;
