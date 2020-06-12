'use strict';

var _user = require('../definition/user.definition');

var _user2 = _interopRequireDefault(_user);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var userDefinition = new _user2.default();

module.exports = function (sequelize) {
    var User = sequelize.define('User', userDefinition, {
        tableName: 'user_account',
        timestamps: false
    });

    User.associate = function (models) {
        User.hasOne(models.Address, {
            foreignKey: 'id_user',
            as: 'address',
            onDelete: 'CASCADE'
        });
    };

    User.prototype.toJSON = userDefinition.toJSON;

    return User;
};
//# sourceMappingURL=user.model.js.map
