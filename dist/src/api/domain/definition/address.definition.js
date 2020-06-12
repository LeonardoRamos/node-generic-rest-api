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
    _inherits(AddressDefinition, _BaseEntity);

    function AddressDefinition() {
        _classCallCheck(this, AddressDefinition);

        var _this = _possibleConstructorReturn(this, (AddressDefinition.__proto__ || Object.getPrototypeOf(AddressDefinition)).call(this));

        _this.street = {
            type: _sequelize2.default.STRING
        };
        _this.streetNumber = {
            type: _sequelize2.default.STRING,
            field: 'street_number'
        };
        _this.state = {
            type: _sequelize2.default.STRING
        };
        return _this;
    }

    return AddressDefinition;
}(_base2.default);
//# sourceMappingURL=address.definition.js.map
