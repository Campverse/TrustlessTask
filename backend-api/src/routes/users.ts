import { Router } from 'express';
import { UserModel } from '../models/User.js';

const router = Router();

router.get('/:address/profile', async (req, res) => {
  try {
    const { address } = req.params;
    
    if (!address || address.trim().length === 0) {
      return res.status(400).json({ error: 'Address is required' });
    }
    
    const profile = await UserModel.getProfile(address);
    res.json(profile);
  } catch (error) {
    console.error('Error fetching user profile:', error);
    res.status(500).json({ error: 'Failed to fetch user profile' });
  }
});

export default router;
