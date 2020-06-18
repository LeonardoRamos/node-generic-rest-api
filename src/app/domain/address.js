import Sequelize from 'sequelize';
import BaseEntity from './core/base.entity';

class AddressDefinition extends BaseEntity {
    constructor() {
        super();
        
        this.street = {
            type: Sequelize.STRING
        };
        this.streetNumber = {
            type: Sequelize.STRING,
            field: 'street_number'
        };
        this.state = {
            type: Sequelize.STRING
        };
    }
}

const addressDefinition = new AddressDefinition();

module.exports = (sequelize) => {
    const Address = sequelize.define('Address', addressDefinition, {
        tableName: 'address',
        timestamps: false
    });

    Address.associate = (models) => {
        Address.belongsTo(models.Country, {
            foreignKey: 'id_country',
            as: 'country',
        });
    };

    Address.prototype.toJSON = addressDefinition.toJSON;

    return Address;
};
