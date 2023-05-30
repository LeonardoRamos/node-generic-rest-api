import { db } from 'generic-rest-express-lib';
import path from 'path';

db.loadModels(path.join(__dirname, '..')).forEach((modelFile) => {
    console.info(`importing model file ${modelFile}`);
    const model = db.sequelize.import(modelFile);
    db[model.name] = model;
});

db.syncModels();

export default db;
