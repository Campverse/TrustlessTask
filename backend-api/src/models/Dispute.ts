import { db } from '../db/database.js';
import { v4 as uuidv4 } from 'uuid';
import { ProjectModel } from './Project.js';

export interface Dispute {
  id: string;
  projectId: string;
  reason: string;
  raiserAddress: string;
  arbiterAddress?: string;
  resolved: boolean;
  outcome?: boolean;
  createdAt: string;
}

export class DisputeModel {
  static async create(data: { projectId: string; reason: string; raiserAddress: string }) {
    await db.read();
    
    const id = uuidv4();
    const createdAt = new Date().toISOString();

    const dispute: Dispute = {
      id,
      projectId: data.projectId,
      reason: data.reason,
      raiserAddress: data.raiserAddress,
      resolved: false,
      createdAt,
    };

    db.data!.disputes.push(dispute);
    await db.write();

    // Update project status
    await ProjectModel.updateStatus(data.projectId, 'Disputed');

    return dispute;
  }

  static async getById(id: string) {
    await db.read();
    return db.data!.disputes.find(d => d.id === id) || null;
  }

  static async resolve(id: string, outcome: boolean) {
    await db.read();
    const dispute = db.data!.disputes.find(d => d.id === id);
    if (dispute) {
      dispute.resolved = true;
      dispute.outcome = outcome;
      await db.write();
    }
    return { txHash: `tx_resolve_${Date.now()}`, status: 'Submitted' };
  }
}
