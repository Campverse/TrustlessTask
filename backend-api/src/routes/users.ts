import { Router } from 'express';
import { UserModel } from '../models/User.js';

const router = Router();

router.get('/:address/profile', async (req, res) => {
  const profile = await UserModel.getProfile(req.params.address);
  res.json(profile);
});

export default router;
