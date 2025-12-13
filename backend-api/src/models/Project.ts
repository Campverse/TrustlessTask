import { db } from '../db/database.js';
import { v4 as uuidv4 } from 'uuid';

export interface Project {
  id: string;
  title: string;
  description: string;
  clientAddress: string;
  freelancerAddress: string;
  totalAmount: number;
  status: string;
  createdAt: string;
  txHash?: string;
  arbiterAddress?: string;
  milestones: Milestone[];
}

export interface Milestone {
  id: number;
  description: string;
  amount: number;
  deadline: string;
  completed: boolean;
  approved: boolean;
  completionTxHash?: string;
  approvalTxHash?: string;
}

export class ProjectModel {
  static async create(data: {
    title: string;
    description: string;
    clientAddress: string;
    freelancerAddress: string;
    totalAmount: number;
    arbiterAddress?: string;
    milestones: Array<{
      description: string;
      amount: number;
      deadline: string;
    }>;
  }) {
    await db.read();
    
    // Validate milestone amounts sum to total
    const milestonesTotal = data.milestones.reduce((sum, m) => sum + m.amount, 0);
    if (Math.abs(milestonesTotal - data.totalAmount) > 1) {
      throw new Error('Milestone amounts must sum to total amount');
    }
    
    // Validate addresses are different
    if (data.clientAddress === data.freelancerAddress) {
      throw new Error('Client and freelancer addresses must be different');
    }
    
    // Validate deadlines are in the future
    const now = new Date();
    for (const milestone of data.milestones) {
      const deadline = new Date(milestone.deadline);
      if (deadline <= now) {
        throw new Error('All milestone deadlines must be in the future');
      }
    }
    
    const id = uuidv4();
    const createdAt = new Date().toISOString();
    const txHash = `tx_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;

    const project: Project = {
      id,
      title: data.title.trim(),
      description: data.description.trim(),
      clientAddress: data.clientAddress.trim(),
      freelancerAddress: data.freelancerAddress.trim(),
      totalAmount: data.totalAmount,
      status: 'Created',
      createdAt,
      txHash,
      arbiterAddress: data.arbiterAddress?.trim(),
      milestones: data.milestones.map((m, index) => ({
        id: index + 1,
        description: m.description.trim(),
        amount: m.amount,
        deadline: m.deadline,
        completed: false,
        approved: false,
      })),
    };

    db.data!.projects.push(project);
    await db.write();

    return project;
  }

  static async getAll() {
    await db.read();
    return db.data!.projects.sort((a, b) => 
      new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime()
    );
  }

  static async getById(id: string) {
    await db.read();
    return db.data!.projects.find(p => p.id === id) || null;
  }

  static async getByAddress(address: string) {
    await db.read();
    return db.data!.projects.filter(
      p => p.clientAddress === address || p.freelancerAddress === address
    ).sort((a, b) => new Date(b.createdAt).getTime() - new Date(a.createdAt).getTime());
  }

  static async updateStatus(id: string, status: string) {
    await db.read();
    const project = db.data!.projects.find(p => p.id === id);
    if (project) {
      project.status = status;
      await db.write();
    }
  }

  static async completeMilestone(projectId: string, milestoneId: number, txHash?: string) {
    await db.read();
    const project = db.data!.projects.find(p => p.id === projectId);
    if (!project) {
      throw new Error('Project not found');
    }
    
    const milestone = project.milestones.find(m => m.id === milestoneId);
    if (!milestone) {
      throw new Error('Milestone not found');
    }
    
    milestone.completed = true;
    if (txHash) {
      milestone.completionTxHash = txHash;
    }
    
    project.status = 'InProgress';
    await db.write();
    
    return { 
      txHash: txHash || `tx_complete_${Date.now()}`, 
      status: 'Completed',
      milestone 
    };
  }

  static async approveMilestone(projectId: string, milestoneId: number, txHash?: string) {
    await db.read();
    const project = db.data!.projects.find(p => p.id === projectId);
    if (!project) {
      throw new Error('Project not found');
    }
    
    const milestone = project.milestones.find(m => m.id === milestoneId);
    if (!milestone) {
      throw new Error('Milestone not found');
    }
    
    if (!milestone.completed) {
      throw new Error('Cannot approve milestone that is not completed');
    }
    
    milestone.approved = true;
    if (txHash) {
      milestone.approvalTxHash = txHash;
    }
    
    // Check if all milestones are completed and approved
    const allApproved = project.milestones.every(m => m.completed && m.approved);
    project.status = allApproved ? 'Completed' : 'InProgress';
    
    await db.write();
    
    return { 
      txHash: txHash || `tx_approve_${Date.now()}`, 
      status: 'Approved',
      milestone,
      projectCompleted: allApproved
    };
  }

  static async cancelProject(id: string) {
    await this.updateStatus(id, 'Cancelled');
    return { txHash: `tx_cancel_${Date.now()}`, status: 'Submitted' };
  }
}
