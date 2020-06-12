import AddressDefinition from '../definition/address.definition';

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
