import express from 'express';
import appPackage from '../../../../package.json';
import dbHealth from '../../config/db.health'

const router = express.Router(); 

router.get('/health', async (req, res) => {
    let db = await dbHealth.doHealthCheck();

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
