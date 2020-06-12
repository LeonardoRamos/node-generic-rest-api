import BaseEntity from '../core/base.entity';
import Sequelize from 'sequelize';

module.exports = class CountryDefinition extends BaseEntity {
    
    constructor() {
        super();
        
        this.name = {
            type: Sequelize.STRING
        };
    }
}