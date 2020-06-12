'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _express = require('express');

var _express2 = _interopRequireDefault(_express);

var _morgan = require('morgan');

var _morgan2 = _interopRequireDefault(_morgan);

var _bodyParser = require('body-parser');

var _bodyParser2 = _interopRequireDefault(_bodyParser);

var _cookieParser = require('cookie-parser');

var _cookieParser2 = _interopRequireDefault(_cookieParser);

var _compression = require('compression');

var _compression2 = _interopRequireDefault(_compression);

var _methodOverride = require('method-override');

var _methodOverride2 = _interopRequireDefault(_methodOverride);

var _cors = require('cors');

var _cors2 = _interopRequireDefault(_cors);

var _httpStatus = require('http-status');

var _httpStatus2 = _interopRequireDefault(_httpStatus);

var _expressWinston = require('express-winston');

var _expressWinston2 = _interopRequireDefault(_expressWinston);

var _expressValidation = require('express-validation');

var _expressValidation2 = _interopRequireDefault(_expressValidation);

var _helmet = require('helmet');

var _helmet2 = _interopRequireDefault(_helmet);

var _winston = require('./winston');

var _winston2 = _interopRequireDefault(_winston);

var _index = require('../routes/core/index.route');

var _index2 = _interopRequireDefault(_index);

var _api = require('../routes/core/api.route');

var _api2 = _interopRequireDefault(_api);

var _config = require('./config');

var _config2 = _interopRequireDefault(_config);

var _api3 = require('../helpers/api.error');

var _api4 = _interopRequireDefault(_api3);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var app = (0, _express2.default)();

if (_config2.default.env === 'development') {
    app.use((0, _morgan2.default)('dev'));
}

app.use(_bodyParser2.default.json());
app.use(_bodyParser2.default.urlencoded({ extended: true }));

app.use((0, _cookieParser2.default)());
app.use((0, _compression2.default)());
app.use((0, _methodOverride2.default)());

app.use((0, _helmet2.default)());

app.use((0, _cors2.default)());

if (_config2.default.env === 'development') {
    _expressWinston2.default.requestWhitelist.push('body');
    _expressWinston2.default.responseWhitelist.push('body');
    app.use(_expressWinston2.default.logger({
        winstonInstance: _winston2.default,
        meta: true,
        msg: 'HTTP {{req.method}} {{req.url}} {{res.statusCode}} {{res.responseTime}}ms',
        colorStatus: true
    }));
}

app.use(_index2.default);
app.use('/v1', _api2.default);

app.use(function (err, req, res, next) {
    if (err instanceof _expressValidation2.default.ValidationError) {
        var unifiedErrorMessage = err.errors.map(function (error) {
            return error.messages.join('. ');
        }).join(' and ');
        var error = new _api4.default(unifiedErrorMessage, err.status, true);

        return next(error);
    } else if (!(err instanceof _api4.default)) {
        var apiError = new _api4.default(err.message, err.status, err.isPublic);
        return next(apiError);
    }

    return next(err);
});

app.use(function (req, res, next) {
    var err = new _api4.default('API not found', _httpStatus2.default.NOT_FOUND);
    return next(err);
});

if (_config2.default.env !== 'test') {
    app.use(_expressWinston2.default.errorLogger({
        winstonInstance: _winston2.default
    }));
}

app.use(function (err, req, res, next) {
    return res.status(err.status).json({
        message: err.isPublic ? err.message : _httpStatus2.default[err.status],
        stack: _config2.default.env === 'development' ? err.stack : {}
    });
});

exports.default = app;
module.exports = exports['default'];
//# sourceMappingURL=express.js.map
