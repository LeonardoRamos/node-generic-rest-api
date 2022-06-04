import Sequelize from 'sequelize';

module.exports = class BaseEntity {

    constructor() {
        this.id = {
            type: Sequelize.INTEGER,
            autoIncrement: true,
            primaryKey: true
        };
        this.externalId = {
            type: Sequelize.STRING(32),
            allowNull: false,
            unique: true,
            field: 'external_id'
        };
        this.active = {
            type: Sequelize.BOOLEAN,
            allowNull: false,
            default: true
        };
        this.insertDate = {
            type: Sequelize.DATE,
            allowNull: false,
            field: 'creation_date'
        };
        this.updateDate = {
            type: Sequelize.DATE,
            allowNull: false,
            field: 'update_date'
        };
        this.deleteDate = {
            type: Sequelize.DATE,
            allowNull: true,
            field: 'delete_date'
        };
    }

    toJSON() {
        let attributes = { ...{}, ...this.get() };

        Object.keys(attributes).forEach(key => 
            (key === 'id' || key.startsWith('id_') || attributes[key] === null) && delete attributes[key]
        );

        return attributes;       
    }
}