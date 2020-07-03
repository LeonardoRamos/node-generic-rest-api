
function mapDialect(dialect) {
    if (dialect === 'postgres') return 'PostgreSQL';
    else if (dialect === 'mysql') return 'MySQL'
    else if (dialect === 'mariadb') return 'MariaDB'
    else if (dialect === 'sqlite') return 'SQLite'
    else if (dialect === 'mssql') return 'Microsoft SQL Server'
    else return 'unknown';
}

export default { mapDialect };
