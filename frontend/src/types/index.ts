export interface Project {
  id: string;
  title: string;
  description: string;
  clientAddress: string;
  freelancerAddress: string;
  totalAmount: number;
  status: ProjectStatus;
  milestones: Milestone[];
  createdAt: string;
  txHash?: string;
}

export type ProjectStatus = 
  | 'Created' 
  | 'InProgress' 
  | 'UnderReview' 
  | 'Disputed' 
  | 'Completed' 
  | 'Cancelled';

export interface Milestone {
  id: number;
  description: string;
  amount: number;
  deadline: string;
  completed: boolean;
  approved: boolean;
}

export interface CreateProjectRequest {
  title: string;
  description: string;
  clientAddress: string;
  freelancerAddress: string;
  totalAmount: number;
  milestones: MilestoneRequest[];
  arbiterAddress?: string;
}

export interface MilestoneRequest {
  description: string;
  amount: number;
  deadline: string;
}

export interface UserProfile {
  address: string;
  completedProjects: number;
  totalEarned: number;
  averageRating: number;
  disputes: number;
}

export interface Dispute {
  id: string;
  projectId: string;
  reason: string;
  raiserAddress: string;
  arbiterAddress: string;
  resolved: boolean;
  outcome?: boolean;
}

export interface WalletState {
  connected: boolean;
  address: string | null;
  balance: number;
  lucid: any | null;
}
