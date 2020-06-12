'use strict';

var _base = require('../core/base.entity');

var _base2 = _interopRequireDefault(_base);

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

function _possibleConstructorReturn(self, call) { if (!self) { throw new ReferenceError("this hasn't been initialised - super() hasn't been called"); } return call && (typeof call === "object" || typeof call === "function") ? call : self; }

function _inherits(subClass, superClass) { if (typeof superClass !== "function" && superClass !== null) { throw new TypeError("Super expression must either be null or a function, not " + typeof superClass); } subClass.prototype = Object.create(superClass && superClass.prototype, { constructor: { value: subClass, enumerable: false, writable: true, configurable: true } }); if (superClass) Object.setPrototypeOf ? Object.setPrototypeOf(subClass, superClass) : subClass.__proto__ = superClass; }

module.exports = function (_BaseEntity) {
    _inherits(UserDefinition, _BaseEntity);

    function UserDefinition() {
        _classCallCheck(this, UserDefinition);

        var _this = _possibleConstructorReturn(this, (UserDefinition.__proto__ || Object.getPrototypeOf(UserDefinition)).call(this));

        _this.name = {
            type: _sequelize2.default.STRING
        };
        _this.email = {
            type: _sequelize2.default.STRING,
            unique: true,
            allowNull: false
        };
        _this.password = {
            type: _sequelize2.default.STRING,
            allowNull: false
        };
        _this.role = {
            type: _sequelize2.default.ENUM('ADMIN', 'USER'),
            allowNull: false
        };
        return _this;
    }

    return UserDefinition;
}(_base2.default);
//# sourceMappingURL=user.definition.js.map
