import Sequelize from 'sequelize';
import fs from 'fs';
import path from 'path';
import _ from 'lodash';
import config from './config';
import pg from 'pg';

pg.defaults.parseInt8 = true;

const db = {};

const sequelize = new Sequelize(
    config.postgres.db,
    config.postgres.user,
    config.postgres.password,
    {
        dialect: 'postgres',
        port: config.postgres.port,
        host: config.postgres.host,
    },
);

const modelsDir = path.normalize(`${__dirname}/../domain`);

fs
    .readdirSync(modelsDir, { withFileTypes: true })
    .filter(file => file.name.indexOf('.') !== 0 && file.name.indexOf('.map') === -1 
            && file.name.indexOf('.js') !== -1 && file.isFile())
    .forEach((file) => {
        console.info(`Loading model file ${file}`);
        const model = sequelize.import(path.join(modelsDir, file.name));
        db[model.name] = model;
    });

Object.keys(db).forEach((modelName) => {
    if (db[modelName].associate) {
        db[modelName].associate(db);
    }
});

sequelize.sync().then((result) => {
    console.info('Database synchronized');
}).catch(err => {
    console.log(err);
});;

module.exports = _.extend(
    {
        sequelize,
        Sequelize,
    },
    db,
);
