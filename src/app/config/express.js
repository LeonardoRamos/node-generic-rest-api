import appPackage from '../../../package.json';
import authRoute from '../route/auth.route';
import apiRoutes from '../route/api/api.route';
import { app } from 'generic-rest-express-lib';

app.use('/auth', authRoute);
app.use('/v1', apiRoutes);

app.setupApp(appPackage);

export default app;
