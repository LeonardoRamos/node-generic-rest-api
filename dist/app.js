'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

require('babel-polyfill');

var _config = require('./src/api/config/config');

var _config2 = _interopRequireDefault(_config);

var _express = require('./src/api/config/express');

var _express2 = _interopRequireDefault(_express);

var _sequelize = require('./src/api/config/sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var debug = require('debug')('express-postgresql-sequelize-es6-rest-api:index');

Promise = require('bluebird');

if (!module.parent) {
    _express2.default.listen(_config2.default.port, function () {
        console.info('server started on port ' + _config2.default.port + ' (' + _config2.default.env + ')');
    });
}

exports.default = _express2.default;
module.exports = exports['default'];
//# sourceMappingURL=app.js.map
