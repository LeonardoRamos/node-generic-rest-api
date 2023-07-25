import { BaseApiEntity, db } from 'generic-rest-express-lib';

class AddressDefinition extends BaseApiEntity {
    
    constructor() {
        super();
        
        this.street = {
            type: db.Sequelize.STRING
        };
        this.streetNumber = {
            type: db.Sequelize.STRING,
            field: 'street_number'
        };
        this.state = {
            type: db.Sequelize.STRING
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
