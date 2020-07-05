import express from 'express';
import appPackage from '../../../../package.json';
import dbHealth from '../../config/health/db.health'

const router = express.Router(); 

router.get('/health', async (req, res) => {
    let status = 'UP';
    
    let db = await dbHealth.doHealthCheck();
    status = db.status === 'UP' ? status : 'DOWN'

    res.send({
        status,
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
