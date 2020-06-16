import Sequelize from 'sequelize';
import BaseEntity from './core/base.entity';

class CountryDefinition extends BaseEntity {
    
    constructor() {
        super();
        
        this.name = {
            type: Sequelize.STRING
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
