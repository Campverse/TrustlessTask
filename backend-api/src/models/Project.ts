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
    
    const id = uuidv4();
    const createdAt = new Date().toISOString();
    const txHash = `tx_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`;

    const project: Project = {
      id,
      title: data.title,
      description: data.description,
      clientAddress: data.clientAddress,
      freelancerAddress: data.freelancerAddress,
      totalAmount: data.totalAmount,
      status: 'Created',
      createdAt,
      txHash,
      arbiterAddress: data.arbiterAddress,
      milestones: data.milestones.map((m, index) => ({
        id: index + 1,
        description: m.description,
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

  static async completeMilestone(projectId: string, milestoneId: number) {
    await db.read();
    const project = db.data!.projects.find(p => p.id === projectId);
    if (project) {
      const milestone = project.milestones.find(m => m.id === milestoneId);
      if (milestone) {
        milestone.completed = true;
        await db.write();
      }
    }
    return { txHash: `tx_complete_${Date.now()}`, status: 'Submitted' };
  }

  static async approveMilestone(projectId: string, milestoneId: number) {
    await db.read();
    const project = db.data!.projects.find(p => p.id === projectId);
    if (project) {
      const milestone = project.milestones.find(m => m.id === milestoneId);
      if (milestone) {
        milestone.approved = true;
        
        const allApproved = project.milestones.every(m => m.completed && m.approved);
        project.status = allApproved ? 'Completed' : 'InProgress';
        
        await db.write();
      }
    }
    return { txHash: `tx_approve_${Date.now()}`, status: 'Submitted' };
  }

  static async cancelProject(id: string) {
    await this.updateStatus(id, 'Cancelled');
    return { txHash: `tx_cancel_${Date.now()}`, status: 'Submitted' };
  }
}
