import express from 'express';
import appPackage from '../../../../package.json';
import databaseHealth from '../../config/database.health'

const router = express.Router(); 

router.get('/health', async (req, res) => {
    let db = await databaseHealth.doHealthCheck();

    res.send({
        status: db.status === 'UP' ? 'UP' : 'DOWN',
        components: { db }
    });
});

router.get('/info', (req, res) => {
    res.send({
        name: appPackage.name,
        description: appPackage.description,
        version: appPackage.version
    })
});

export default router;
