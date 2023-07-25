import { BaseApiEntity, db } from 'generic-rest-express-lib';

class CountryDefinition extends BaseApiEntity {

    constructor() {
        super();
        
        this.name = {
            type: db.Sequelize.STRING
        };
    }
    
}

const countryDefinition = new CountryDefinition();

module.exports = (sequelize) => {
    const Country = sequelize.define('Country', countryDefinition, {
        tableName: 'country',
        timestamps: false
    });

    Country.prototype.toJSON = countryDefinition.toJSON;

    return Country;
};
