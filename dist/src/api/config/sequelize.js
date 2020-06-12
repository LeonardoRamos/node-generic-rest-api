'use strict';

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

var _fs = require('fs');

var _fs2 = _interopRequireDefault(_fs);

var _path = require('path');

var _path2 = _interopRequireDefault(_path);

var _lodash = require('lodash');

var _lodash2 = _interopRequireDefault(_lodash);

var _config = require('./config');

var _config2 = _interopRequireDefault(_config);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var db = {};

// connect to postgres db
var sequelize = new _sequelize2.default(_config2.default.postgres.db, _config2.default.postgres.user, _config2.default.postgres.password, {
    dialect: 'postgres',
    port: _config2.default.postgres.port,
    host: _config2.default.postgres.host
});

var modelsDir = _path2.default.normalize(__dirname + '/../domain/model');

// loop through all files in models directory ignoring hidden files and this file
_fs2.default.readdirSync(modelsDir).filter(function (file) {
    return file.indexOf('.') !== 0 && file.indexOf('.map') === -1;
})
// import model files and save model names
.forEach(function (file) {
    console.info('Loading model file ' + file);
    var model = sequelize.import(_path2.default.join(modelsDir, file));
    db[model.name] = model;
});

// calling all the associate function, in order to make the association between the models
Object.keys(db).forEach(function (modelName) {
    if (db[modelName].associate) {
        db[modelName].associate(db);
    }
});

// Synchronizing any model changes with database.
sequelize.sync().then(function (result) {
    console.info('Database synchronized');
}).catch(function (err) {
    console.log(err);
});;

// assign the sequelize variables to the db object and returning the db.
module.exports = _lodash2.default.extend({
    sequelize: sequelize,
    Sequelize: _sequelize2.default
}, db);
//# sourceMappingURL=sequelize.js.map
