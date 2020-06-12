'use strict';

var _address = require('../definition/address.definition');

var _address2 = _interopRequireDefault(_address);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var addressDefinition = new _address2.default();

module.exports = function (sequelize) {
    var Address = sequelize.define('Address', addressDefinition, {
        tableName: 'address',
        timestamps: false
    });

    Address.associate = function (models) {
        Address.belongsTo(models.Country, {
            foreignKey: 'id_country',
            as: 'country'
        });
    };

    Address.prototype.toJSON = addressDefinition.toJSON;

    return Address;
};
//# sourceMappingURL=address.model.js.map
