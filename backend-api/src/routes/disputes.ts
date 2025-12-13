import { Router } from 'express';
import { DisputeModel } from '../models/Dispute.js';
import { z } from 'zod';

const router = Router();

const createDisputeSchema = z.object({
  projectId: z.string(),
  reason: z.string().min(1),
  raiserAddress: z.string().min(1),
});

router.post('/', async (req, res) => {
  try {
    console.log('📥 Creating dispute:', req.body);
    const data = createDisputeSchema.parse(req.body);
    const dispute = await DisputeModel.create(data);
    console.log('✅ Dispute created:', dispute.id);
    res.status(201).json(dispute);
  } catch (error) {
    console.error('❌ Error creating dispute:', error);
    res.status(400).json({ 
      error: 'Invalid request data',
      details: error instanceof Error ? error.message : 'Unknown error'
    });
  }
});

router.get('/:id', async (req, res) => {
  try {
    const dispute = await DisputeModel.getById(req.params.id);
    if (!dispute) {
      return res.status(404).json({ error: 'Dispute not found' });
    }
    res.json(dispute);
  } catch (error) {
    console.error('Error fetching dispute:', error);
    res.status(500).json({ error: 'Failed to fetch dispute' });
  }
});

router.post('/:id/resolve', async (req, res) => {
  try {
    const { id } = req.params;
    const outcome = req.body.outcome !== undefined ? req.body.outcome : req.body;
    
    if (typeof outcome !== 'boolean') {
      return res.status(400).json({ error: 'Outcome must be a boolean value' });
    }
    
    const result = await DisputeModel.resolve(id, outcome);
    res.json(result);
  } catch (error) {
    console.error('Error resolving dispute:', error);
    res.status(500).json({ error: 'Failed to resolve dispute' });
  }
});

export default router;
