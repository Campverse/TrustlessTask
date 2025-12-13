import axios from 'axios';
import type { Project, CreateProjectRequest, UserProfile, Dispute } from '../types';
import { mockProjects, mockUserProfile } from './mockData';

const API_BASE_URL = import.meta.env.VITE_API_URL || 'http://localhost:8080/api/v1';
const USE_MOCK_DATA = false; // Real backend API enabled

const api = axios.create({
  baseURL: API_BASE_URL,
  headers: {
    'Content-Type': 'application/json',
  },
});

export const projectsApi = {
  create: async (data: CreateProjectRequest): Promise<Project> => {
    if (USE_MOCK_DATA) {
      // Simulate API delay
      await new Promise(resolve => setTimeout(resolve, 1000));
      const newProject: Project = {
        id: String(mockProjects.length + 1),
        title: data.title,
        description: data.description,
        clientAddress: data.clientAddress,
        freelancerAddress: data.freelancerAddress,
        totalAmount: data.totalAmount,
        status: 'Created',
        milestones: data.milestones.map((m, i) => ({
          id: i + 1,
          description: m.description,
          amount: m.amount,
          deadline: m.deadline,
          completed: false,
          approved: false,
        })),
        createdAt: new Date().toISOString(),
        txHash: 'mock_tx_' + Date.now(),
      };
      mockProjects.push(newProject);
      return newProject;
    }
    
    console.log('📤 Sending to API:', API_BASE_URL + '/projects');
    console.log('📦 Data:', JSON.stringify(data, null, 2));
    
    try {
      const response = await api.post('/projects', data);
      console.log('✅ API Response:', response.data);
      return response.data;
    } catch (error: any) {
      console.error('❌ API Error:', error.response?.data || error.message);
      throw error;
    }
  },

  list: async (): Promise<Project[]> => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 500));
      return mockProjects;
    }
    const response = await api.get('/projects');
    return response.data;
  },

  getById: async (id: string): Promise<Project> => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 300));
      const project = mockProjects.find(p => p.id === id);
      if (!project) throw new Error('Project not found');
      return project;
    }
    const response = await api.get(`/projects/${id}`);
    return response.data;
  },

  completeMilestone: async (projectId: string, milestoneId: number, signedTx?: string) => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 800));
      const project = mockProjects.find(p => p.id === projectId);
      if (project) {
        const milestone = project.milestones.find(m => m.id === milestoneId);
        if (milestone) milestone.completed = true;
      }
      return { txHash: 'mock_complete_' + Date.now(), status: 'Submitted' };
    }
    const response = await api.post(`/projects/${projectId}/milestone/${milestoneId}/complete`, {
      signedTx,
    });
    return response.data;
  },

  approveMilestone: async (projectId: string, milestoneId: number, signedTx?: string) => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 800));
      const project = mockProjects.find(p => p.id === projectId);
      if (project) {
        const milestone = project.milestones.find(m => m.id === milestoneId);
        if (milestone) milestone.approved = true;
      }
      return { txHash: 'mock_approve_' + Date.now(), status: 'Submitted' };
    }
    const response = await api.post(`/projects/${projectId}/milestone/${milestoneId}/approve`, {
      signedTx,
    });
    return response.data;
  },

  cancel: async (projectId: string) => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 800));
      return { txHash: 'mock_cancel_' + Date.now(), status: 'Submitted' };
    }
    const response = await api.post(`/projects/${projectId}/cancel`);
    return response.data;
  },
};

export const usersApi = {
  getProfile: async (address: string): Promise<UserProfile> => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 400));
      return mockUserProfile;
    }
    const response = await api.get(`/users/${address}/profile`);
    return response.data;
  },
};

export const disputesApi = {
  create: async (data: { projectId: string; reason: string; raiserAddress: string }): Promise<Dispute> => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 800));
      return {
        id: 'dispute_' + Date.now(),
        projectId: data.projectId,
        reason: data.reason,
        raiserAddress: data.raiserAddress,
        arbiterAddress: 'addr_test1arbiter...',
        resolved: false,
        outcome: undefined,
      };
    }
    const response = await api.post('/disputes', data);
    return response.data;
  },

  getById: async (id: string): Promise<Dispute> => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 300));
      return {
        id,
        projectId: '1',
        reason: 'Mock dispute',
        raiserAddress: 'addr_test1...',
        arbiterAddress: 'addr_test1arbiter...',
        resolved: false,
        outcome: undefined,
      };
    }
    const response = await api.get(`/disputes/${id}`);
    return response.data;
  },

  resolve: async (id: string, outcome: boolean) => {
    if (USE_MOCK_DATA) {
      await new Promise(resolve => setTimeout(resolve, 800));
      return { txHash: 'mock_resolve_' + Date.now(), status: 'Submitted' };
    }
    const response = await api.post(`/disputes/${id}/resolve`, outcome);
    return response.data;
  },
};
