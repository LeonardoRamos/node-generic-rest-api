'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var login = function () {
    var _ref = _asyncToGenerator( /*#__PURE__*/regeneratorRuntime.mark(function _callee(req, res, next) {
        var user, token, err;
        return regeneratorRuntime.wrap(function _callee$(_context) {
            while (1) {
                switch (_context.prev = _context.next) {
                    case 0:
                        _context.next = 2;
                        return userService.findByEmail(req.body.email);

                    case 2:
                        user = _context.sent;

                        if (!(user && _bcrypt2.default.compareSync(req.body.password, user.password))) {
                            _context.next = 6;
                            break;
                        }

                        token = _jsonwebtoken2.default.sign({
                            slug: user.slug,
                            name: user.name,
                            email: user.email,
                            role: user.role,
                            expiresIn: _config2.default.jwtExpiration
                        }, _config2.default.jwtSecret);
                        return _context.abrupt('return', res.json({ token: token }));

                    case 6:
                        err = new _api2.default('Authentication error', _httpStatus2.default.UNAUTHORIZED, true);
                        return _context.abrupt('return', next(err));

                    case 8:
                    case 'end':
                        return _context.stop();
                }
            }
        }, _callee, this);
    }));

    return function login(_x, _x2, _x3) {
        return _ref.apply(this, arguments);
    };
}();

var _jsonwebtoken = require('jsonwebtoken');

var _jsonwebtoken2 = _interopRequireDefault(_jsonwebtoken);

var _bcrypt = require('bcrypt');

var _bcrypt2 = _interopRequireDefault(_bcrypt);

var _joi = require('joi');

var _joi2 = _interopRequireDefault(_joi);

var _httpStatus = require('http-status');

var _httpStatus2 = _interopRequireDefault(_httpStatus);

var _api = require('../helpers/api.error');

var _api2 = _interopRequireDefault(_api);

var _config = require('../config/config');

var _config2 = _interopRequireDefault(_config);

var _user = require('../service/user.service');

var _user2 = _interopRequireDefault(_user);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _asyncToGenerator(fn) { return function () { var gen = fn.apply(this, arguments); return new Promise(function (resolve, reject) { function step(key, arg) { try { var info = gen[key](arg); var value = info.value; } catch (error) { reject(error); return; } if (info.done) { resolve(value); } else { return Promise.resolve(value).then(function (value) { step("next", value); }, function (err) { step("throw", err); }); } } return step("next"); }); }; }

var userService = new _user2.default();

function paramValidation() {
    return {
        login: {
            body: {
                email: _joi2.default.string().required(),
                password: _joi2.default.string().required()
            }
        }
    };
}

exports.default = { login: login, paramValidation: paramValidation };
module.exports = exports['default'];
//# sourceMappingURL=auth.controller.js.map
