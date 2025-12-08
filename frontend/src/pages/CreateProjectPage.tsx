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
      navigate(`/project/${project.id}`);
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
          onSubmit={(data: CreateProjectRequest) => createMutation.mutate(data)}
        />
      </div>

      {createMutation.isPending && (
        <div className="mt-4 text-center text-gray-600">
          Creating project and submitting transaction...
        </div>
      )}

      {createMutation.isError && (
        <div className="mt-4 p-4 bg-red-100 text-red-700 rounded-lg">
          Error creating project. Please try again.
        </div>
      )}
    </div>
  );
};
