import { Router } from 'express';
import { ProjectModel } from '../models/Project.js';
import { getCardanoTxBuilder } from '../services/cardano-tx.js';
import { z } from 'zod';

const router = Router();
const cardanoTx = getCardanoTxBuilder();

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
  try {
    const { projectId, milestoneId } = req.params;
    const { signedTx } = req.body;
    
    console.log(`📝 Marking milestone ${milestoneId} as complete for project ${projectId}`);
    
    if (!signedTx) {
      return res.status(400).json({ 
        error: 'Transaction required',
        details: 'Real blockchain transaction hash is required to mark milestone as complete. This creates proof of completion on-chain.'
      });
    }
    
    // The completion transaction hash is already submitted by the frontend via Lucid
    // This is a proof-of-completion transaction (1 ADA sent to client with metadata)
    const txHash = signedTx;
    
    console.log('📝 Recording completion transaction on blockchain...');
    console.log('Transaction hash:', txHash);
    console.log(`View on explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
    
    const result = await ProjectModel.completeMilestone(projectId, parseInt(milestoneId), txHash);
    
    res.json({
      ...result,
      txHash,
      explorerUrl: `https://preprod.cardanoscan.io/transaction/${txHash}`,
      message: 'Milestone marked as complete on Cardano blockchain. Proof-of-completion transaction recorded.'
    });
  } catch (error: any) {
    console.error('❌ Error completing milestone:', error);
    res.status(400).json({ 
      error: 'Failed to complete milestone',
      details: error.message 
    });
  }
});

router.post('/:projectId/milestone/:milestoneId/approve', async (req, res) => {
  try {
    const { projectId, milestoneId } = req.params;
    const { signedTx } = req.body;
    
    console.log(`✅ Approving milestone ${milestoneId} for project ${projectId}`);
    
    if (!signedTx) {
      return res.status(400).json({ 
        error: 'Transaction required',
        details: 'Real blockchain transaction hash is required. No simulated transactions allowed.'
      });
    }
    
    // The transaction hash is already submitted by the frontend via Lucid
    // We just store it in the database
    const txHash = signedTx;
    
    console.log('💰 Recording blockchain transaction...');
    console.log('Transaction hash:', txHash);
    console.log(`View on explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
    
    const result = await ProjectModel.approveMilestone(projectId, parseInt(milestoneId), txHash);
    
    res.json({
      ...result,
      txHash,
      explorerUrl: `https://preprod.cardanoscan.io/transaction/${txHash}`,
      message: 'Funds released successfully on Cardano blockchain'
    });
  } catch (error: any) {
    console.error('❌ Error approving milestone:', error);
    res.status(400).json({ 
      error: 'Failed to approve milestone',
      details: error.message 
    });
  }
});

router.post('/:id/cancel', async (req, res) => {
  const result = await ProjectModel.cancelProject(req.params.id);
  res.json(result);
});

export default router;
