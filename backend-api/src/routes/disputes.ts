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
    const data = createDisputeSchema.parse(req.body);
    const dispute = await DisputeModel.create(data);
    res.status(201).json(dispute);
  } catch (error) {
    res.status(400).json({ error: 'Invalid request data' });
  }
});

router.get('/:id', async (req, res) => {
  const dispute = await DisputeModel.getById(req.params.id);
  if (!dispute) {
    return res.status(404).json({ error: 'Dispute not found' });
  }
  res.json(dispute);
});

router.post('/:id/resolve', async (req, res) => {
  const outcome = req.body.outcome || req.body;
  const result = await DisputeModel.resolve(req.params.id, Boolean(outcome));
  res.json(result);
});

export default router;
