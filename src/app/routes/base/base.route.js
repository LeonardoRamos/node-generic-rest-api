import express from 'express';
import authRoutes from '../auth.route';

const router = express.Router(); 

router.get('/manage/health', (req, res) => res.send({
    status: 'UP',
    uptime: process.uptime(),
    timestamp: Date.now()
}));

router.use('/auth', authRoutes);

export default router;
