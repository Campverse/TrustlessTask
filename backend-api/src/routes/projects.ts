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
    console.log('📥 Received project creation request:', JSON.stringify(req.body, null, 2));
    const data = createProjectSchema.parse(req.body);
    console.log('✅ Validation passed, creating project...');
    const project = await ProjectModel.create(data);
    console.log('✅ Project created:', project.id);
    res.status(201).json(project);
  } catch (error: any) {
    console.error('❌ Error creating project:', error);
    
    // Handle Zod validation errors
    if (error.name === 'ZodError') {
      const validationErrors = error.errors.map((err: any) => ({
        field: err.path.join('.'),
        message: err.message,
        received: err.received,
      }));
      
      console.error('Validation errors:', validationErrors);
      
      return res.status(400).json({ 
        error: 'Validation failed',
        details: 'Please check all required fields',
        validationErrors,
      });
    }
    
    // Handle other errors
    console.error('Error message:', error.message);
    console.error('Error stack:', error.stack);
    
    res.status(400).json({ 
      error: 'Failed to create project',
      details: error.message || 'Unknown error'
    });
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
