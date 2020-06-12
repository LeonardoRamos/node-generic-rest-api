'use strict';

Object.defineProperty(exports, "__esModule", {
    value: true
});

var _express = require('express');

var _express2 = _interopRequireDefault(_express);

var _expressValidation = require('express-validation');

var _expressValidation2 = _interopRequireDefault(_expressValidation);

var _expressJwt = require('express-jwt');

var _expressJwt2 = _interopRequireDefault(_expressJwt);

var _auth = require('../controller/auth.controller');

var _auth2 = _interopRequireDefault(_auth);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var router = _express2.default.Router();
var paramValidation = _auth2.default.paramValidation();

router.route('/login').post((0, _expressValidation2.default)(paramValidation.login), _auth2.default.login);

exports.default = router;
module.exports = exports['default'];
//# sourceMappingURL=auth.route.js.map
