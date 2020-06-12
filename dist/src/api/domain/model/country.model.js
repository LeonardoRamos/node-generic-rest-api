'use strict';

var _country = require('../definition/country.definition');

var _country2 = _interopRequireDefault(_country);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var countryDefinition = new _country2.default();

module.exports = function (sequelize) {
    var Country = sequelize.define('Country', countryDefinition, {
        tableName: 'country',
        timestamps: false
    });

    Country.prototype.toJSON = countryDefinition.toJSON;

    return Country;
};
//# sourceMappingURL=country.model.js.map
