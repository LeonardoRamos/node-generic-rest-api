import express from 'express';
import { sequelize } from '../../config/sequelize';
import authRoutes from '../auth.route';

const router = express.Router(); 

router.get('/manage/health', (req, res) => {
    let healthStatus = {
        status: 'UP',
        uptime: process.uptime(),
        timestamp: Date.now()
    };

    sequelize
        .authenticate()
            .then(() => {
                healthStatus.db = {
                    status: 'UP',
                    database: sequelize.options.dialect
                };
                res.send(healthStatus)
            })
            .catch(err => {
                healthStatus.db = {
                    status: 'DOWN',
                    database: sequelize.options.dialect
                };
                res.send(healthStatus)
            });
});

router.use('/auth', authRoutes);

export default router;
