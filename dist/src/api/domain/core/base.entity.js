'use strict';

var _extends = Object.assign || function (target) { for (var i = 1; i < arguments.length; i++) { var source = arguments[i]; for (var key in source) { if (Object.prototype.hasOwnProperty.call(source, key)) { target[key] = source[key]; } } } return target; };

var _createClass = function () { function defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } } return function (Constructor, protoProps, staticProps) { if (protoProps) defineProperties(Constructor.prototype, protoProps); if (staticProps) defineProperties(Constructor, staticProps); return Constructor; }; }();

var _sequelize = require('sequelize');

var _sequelize2 = _interopRequireDefault(_sequelize);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

module.exports = function () {
    function BaseEntity() {
        _classCallCheck(this, BaseEntity);

        this.id = {
            type: _sequelize2.default.INTEGER,
            autoIncrement: true,
            primaryKey: true
        };
        this.slug = {
            type: _sequelize2.default.STRING(32),
            allowNull: false,
            unique: true
        };
        this.active = {
            type: _sequelize2.default.BOOLEAN,
            allowNull: false,
            default: true
        };
        this.insertDate = {
            type: _sequelize2.default.DATE,
            allowNull: false,
            field: 'creation_date'
        };
        this.updateDate = {
            type: _sequelize2.default.DATE,
            allowNull: false,
            field: 'update_date'
        };
        this.deleteDate = {
            type: _sequelize2.default.DATE,
            allowNull: true,
            field: 'delete_date'
        };
    }

    _createClass(BaseEntity, [{
        key: 'toJSON',
        value: function toJSON() {
            var attributes = _extends({}, this.get());

            Object.keys(attributes).forEach(function (key) {
                return (key === 'id' || key.startsWith('id_') || attributes[key] === null) && delete attributes[key];
            });

            return attributes;
        }
    }]);

    return BaseEntity;
}();
//# sourceMappingURL=base.entity.js.map
