import express from 'express';
import apiPackage from '../../../../package.json';
import { sequelize } from '../../config/sequelize';

const router = express.Router(); 

router.get('/health', (req, res) => {
    let healthStatus = {
        status: 'UP',
        components: {
            db: {
                status: 'UP',
                details: {
                    database: sequelize.options.dialect,
                    validationQuery: 'SELECT 1+1 AS result'
                }
            }
        }
    };

    sequelize
        .authenticate()
            .then(() => {
                res.send(healthStatus)
            })
            .catch(err => {
                healthStatus.status = 'DOWN';
                healthStatus.components.db.status = 'DOWN';
                res.send(healthStatus)
            });
});

router.get('/info', (req, res) => {
    res.send({
        name: apiPackage.name,
        description: apiPackage.description,
        version: apiPackage.version
    })
});

export default router;
