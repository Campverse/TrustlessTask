import { Router } from 'express';
import { ProjectModel } from '../models/Project.js';
import { z } from 'zod';

const router = Router();

const createProjectSchema = z.object({
  title: z.string().min(1),
  description: z.string().min(1),
  clientAddress: z.string().min(1),
  freelancerAddress: z.string().min(1),
  totalAmount: z.number().positive(),
  arbiterAddress: z.string().optional(),
  milestones: z.array(
    z.object({
      description: z.string().min(1),
      amount: z.number().positive(),
      deadline: z.string(),
    })
  ),
});

router.post('/', async (req, res) => {
  try {
    const data = createProjectSchema.parse(req.body);
    const project = await ProjectModel.create(data);
    res.status(201).json(project);
  } catch (error) {
    res.status(400).json({ error: 'Invalid request data' });
  }
});

router.get('/', async (req, res) => {
  const projects = await ProjectModel.getAll();
  res.json(projects);
});

router.get('/:id', async (req, res) => {
  const project = await ProjectModel.getById(req.params.id);
  if (!project) {
    return res.status(404).json({ error: 'Project not found' });
  }
  res.json(project);
});

router.post('/:projectId/milestone/:milestoneId/complete', async (req, res) => {
  const { projectId, milestoneId } = req.params;
  const result = await ProjectModel.completeMilestone(projectId, parseInt(milestoneId));
  res.json(result);
});

router.post('/:projectId/milestone/:milestoneId/approve', async (req, res) => {
  const { projectId, milestoneId } = req.params;
  const result = await ProjectModel.approveMilestone(projectId, parseInt(milestoneId));
  res.json(result);
});

router.post('/:id/cancel', async (req, res) => {
  const result = await ProjectModel.cancelProject(req.params.id);
  res.json(result);
});

export default router;
