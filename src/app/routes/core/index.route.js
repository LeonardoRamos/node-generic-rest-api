import express from 'express';
import authRoutes from '../auth.route';

const router = express.Router(); 

router.get('/manage/health', (req, res) => res.send('OK'));

router.use('/auth', authRoutes);

export default router;
