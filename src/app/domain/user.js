import { BaseApiEntity, db } from 'generic-rest-express-lib';

class UserDefinition extends BaseApiEntity { 
    constructor() {
        super();
        
        this.name = {
            type: db.Sequelize.STRING
        };
        this.email = {
            type: db.Sequelize.STRING,
            unique: true,
            allowNull: false
        };
        this.password = {
            type: db.Sequelize.STRING,
            allowNull: false
        };
        this.role = {
            type: db.Sequelize.ENUM('ADMIN', 'USER'),
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
