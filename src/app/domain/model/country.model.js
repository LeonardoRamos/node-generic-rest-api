import CountryDefinition from '../definition/country.definition';

const countryDefinition = new CountryDefinition();

module.exports = (sequelize) => {
    const Country = sequelize.define('Country', countryDefinition, {
        tableName: 'country',
        timestamps: false
    });

    Country.prototype.toJSON = countryDefinition.toJSON;

    return Country;
};
