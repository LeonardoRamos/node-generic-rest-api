import Sequelize from 'sequelize';
import BaseEntity from './core/base.entity';

class UserDefinition extends BaseEntity { 
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

const userDefinition = new UserDefinition();

module.exports = (sequelize) => {
    const User = sequelize.define('User', userDefinition, {
        tableName: 'user_account',
        timestamps: false
    });

    User.associate = (models) => {
        User.hasOne(models.Address, {
            foreignKey: 'id_user',
            as: 'address',
            onDelete: 'CASCADE'
        });
    };

    User.prototype.toJSON = userDefinition.toJSON;

    return User;
};
