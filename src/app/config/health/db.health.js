import { sequelize } from '../sequelize';
import winstonInstance from '../winston';

async function doHealthCheck() {
    let dbHealth = {
        status: 'UNKNOWN',
        details: {
            database: mapDialectToProduct(sequelize.options.dialect)
        }
    };

    try {
        await sequelize.authenticate();
        dbHealth.status = 'UP';

    } catch (err) {
        winstonInstance.error(err);
        dbHealth.status = 'DOWN';
    }

    return dbHealth;
}

function mapDialectToProduct(dialect) {
    if (dialect === 'postgres') return 'PostgreSQL';
    else if (dialect === 'mysql') return 'MySQL'
    else if (dialect === 'mariadb') return 'MariaDB'
    else if (dialect === 'sqlite') return 'SQLite'
    else if (dialect === 'mssql') return 'Microsoft SQL Server'
    else return 'unknown';
}

export default { doHealthCheck };
