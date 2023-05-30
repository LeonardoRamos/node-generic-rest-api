import '@babel/polyfill';
import { config } from 'generic-rest-express-lib';
import app from './config/express';
import './config/database';

if (require.main === module) {
    app.listen(config.port, () => {
        console.info(`server started on port ${config.port} (${config.env})`);
    });
}

export default app;
