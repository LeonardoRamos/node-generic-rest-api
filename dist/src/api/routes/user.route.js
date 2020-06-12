'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _config = require('../config/config');

var _config2 = _interopRequireDefault(_config);

var _express = require('express');

var _express2 = _interopRequireDefault(_express);

var _expressJwt = require('express-jwt');

var _expressJwt2 = _interopRequireDefault(_expressJwt);

var _expressValidation = require('express-validation');

var _expressValidation2 = _interopRequireDefault(_expressValidation);

var _user = require('../controller/user.controller');

var _user2 = _interopRequireDefault(_user);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var router = _express2.default.Router();
var userController = new _user2.default();
var paramValidation = userController.paramValidation();

router.route('/').get(userController.list).post([(0, _expressJwt2.default)({ secret: _config2.default.jwtSecret }), (0, _expressValidation2.default)(paramValidation.insert)], userController.insert);

router.route('/:slug').get(userController.get).put([(0, _expressJwt2.default)({ secret: _config2.default.jwtSecret }), (0, _expressValidation2.default)(paramValidation.update)], userController.update).delete(userController.remove);

router.param('slug', userController.getBySlug);

exports.default = router;
module.exports = exports['default'];
//# sourceMappingURL=user.route.js.map
