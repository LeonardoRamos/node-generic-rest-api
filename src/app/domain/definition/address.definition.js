import BaseEntity from '../core/base.entity';
import Sequelize from 'sequelize';

module.exports = class AddressDefinition extends BaseEntity {
    
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