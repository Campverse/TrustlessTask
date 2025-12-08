import React from 'react';
import { useNavigate } from 'react-router-dom';
import { useMutation } from '@tanstack/react-query';
import { projectsApi } from '../services/api';
import { CreateProjectForm } from '../components/CreateProjectForm';
import type { WalletState, CreateProjectRequest } from '../types';

interface CreateProjectPageProps {
  wallet: WalletState;
}

export const CreateProjectPage: React.FC<CreateProjectPageProps> = ({ wallet }) => {
  const navigate = useNavigate();

  const createMutation = useMutation({
    mutationFn: projectsApi.create,
    onSuccess: (project) => {
      console.log('✅ Project created successfully:', project);
      alert(`Project "${project.title}" created successfully!`);
      navigate(`/project/${project.id}`);
    },
    onError: (error: any) => {
      console.error('❌ Error creating project:', error);
      console.error('Error details:', error.response?.data || error.message);
    },
  });

  if (!wallet.connected) {
    return (
      <div className="text-center py-12">
        <p className="text-gray-600 mb-4">Please connect your wallet to create a project</p>
      </div>
    );
  }

  return (
    <div className="max-w-3xl mx-auto">
      <h1 className="text-3xl font-bold mb-8">Create New Project</h1>
      
      <div className="bg-white rounded-lg shadow-md p-8">
        <CreateProjectForm
          clientAddress={wallet.address!}
          onSubmit={(data: CreateProjectRequest) => {
            console.log('📤 Submitting project:', data);
            createMutation.mutate(data);
          }}
        />
      </div>

      {createMutation.isPending && (
        <div className="mt-4 text-center text-gray-600">
          Creating project and submitting transaction...
        </div>
      )}

      {createMutation.isError && (
        <div className="mt-4 p-4 bg-red-100 text-red-700 rounded-lg">
          <div className="font-semibold mb-2">Error creating project</div>
          <div className="text-sm">
            {createMutation.error instanceof Error 
              ? createMutation.error.message 
              : 'Unknown error occurred. Please check console for details.'}
          </div>
          <div className="text-xs mt-2">
            Check browser console (F12) for more details.
          </div>
        </div>
      )}

      {createMutation.isSuccess && (
        <div className="mt-4 p-4 bg-green-100 text-green-700 rounded-lg">
          ✅ Project created successfully! Redirecting...
        </div>
      )}
    </div>
  );
};
