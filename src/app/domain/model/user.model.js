import UserDefinition from '../definition/user.definition';

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
