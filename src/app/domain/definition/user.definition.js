import BaseEntity from '../core/base.entity';
import Sequelize from 'sequelize';

module.exports = class UserDefinition extends BaseEntity {
    
    constructor() {
        super();
        
        this.name = {
            type: Sequelize.STRING
        };
        this.email = {
            type: Sequelize.STRING,
            unique: true,
            allowNull: false
        };
        this.password = {
            type: Sequelize.STRING,
            allowNull: false
        };
        this.role = {
            type: Sequelize.ENUM('ADMIN', 'USER'),
            allowNull: false
        };
    }
}