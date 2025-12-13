import React from 'react';
import { useParams } from 'react-router-dom';
import { useQuery, useMutation, useQueryClient } from '@tanstack/react-query';
import { format } from 'date-fns';
import { projectsApi } from '../services/api';
import type { WalletState } from '../types';

interface ProjectDetailPageProps {
  wallet: WalletState;
}

export const ProjectDetailPage: React.FC<ProjectDetailPageProps> = ({ wallet }) => {
  const { id } = useParams<{ id: string }>();
  const queryClient = useQueryClient();

  const { data: project, isLoading, isError, error } = useQuery({
    queryKey: ['project', id],
    queryFn: () => projectsApi.getById(id!),
    enabled: !!id,
    retry: 2,
  });

  const completeMutation = useMutation({
    mutationFn: ({ milestoneId }: { milestoneId: number }) =>
      projectsApi.completeMilestone(id!, milestoneId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['project', id] });
      alert('Milestone marked as complete!');
    },
    onError: (error) => {
      console.error('Failed to complete milestone:', error);
      alert('Failed to complete milestone. Please try again.');
    },
  });

  const approveMutation = useMutation({
    mutationFn: ({ milestoneId }: { milestoneId: number }) =>
      projectsApi.approveMilestone(id!, milestoneId),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ['project', id] });
      alert('Milestone approved and funds released!');
    },
    onError: (error) => {
      console.error('Failed to approve milestone:', error);
      alert('Failed to approve milestone. Please try again.');
    },
  });

  if (isLoading) {
    return (
      <div className="text-center py-12">
        <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
        <p className="mt-4 text-gray-600">Loading project...</p>
      </div>
    );
  }

  if (isError) {
    return (
      <div className="text-center py-12">
        <div className="bg-red-100 text-red-700 p-6 rounded-lg max-w-md mx-auto">
          <h2 className="text-xl font-semibold mb-2">Failed to load project</h2>
          <p className="text-sm">{error instanceof Error ? error.message : 'Unknown error'}</p>
        </div>
      </div>
    );
  }

  if (!project) {
    return (
      <div className="text-center py-12">
        <div className="text-gray-600">Project not found</div>
      </div>
    );
  }

  const isClient = wallet.address === project.clientAddress;
  const isFreelancer = wallet.address === project.freelancerAddress;

  return (
    <div className="max-w-4xl mx-auto">
      <div className="bg-white rounded-lg shadow-md p-8 mb-6">
        <div className="flex justify-between items-start mb-6">
          <div>
            <h1 className="text-3xl font-bold mb-2">{project.title}</h1>
            <span className="px-3 py-1 bg-blue-100 text-blue-800 rounded-full text-sm">
              {project.status}
            </span>
          </div>
          <div className="text-right">
            <div className="text-2xl font-bold text-blue-600">
              {(project.totalAmount / 1_000_000).toFixed(2)} ₳
            </div>
            <div className="text-sm text-gray-500">Total Amount</div>
          </div>
        </div>

        <p className="text-gray-700 mb-6">{project.description}</p>

        <div className="grid grid-cols-2 gap-4 mb-6">
          <div>
            <div className="text-sm text-gray-500">Client</div>
            <div className="font-mono text-sm">
              {project.clientAddress.slice(0, 20)}...
            </div>
          </div>
          <div>
            <div className="text-sm text-gray-500">Freelancer</div>
            <div className="font-mono text-sm">
              {project.freelancerAddress.slice(0, 20)}...
            </div>
          </div>
        </div>

        {project.txHash && (
          <div className="text-sm text-gray-500">
            Transaction: <span className="font-mono">{project.txHash}</span>
          </div>
        )}
      </div>

      <div className="bg-white rounded-lg shadow-md p-8">
        <h2 className="text-2xl font-bold mb-6">Milestones</h2>

        <div className="space-y-4">
          {project.milestones.map((milestone) => (
            <div key={milestone.id} className="border rounded-lg p-6">
              <div className="flex justify-between items-start mb-4">
                <div className="flex-1">
                  <h3 className="text-lg font-semibold mb-2">
                    Milestone {milestone.id}
                  </h3>
                  <p className="text-gray-600">{milestone.description}</p>
                </div>
                <div className="text-right ml-4">
                  <div className="text-xl font-bold text-blue-600">
                    {(milestone.amount / 1_000_000).toFixed(2)} ₳
                  </div>
                  <div className="text-sm text-gray-500">
                    Due: {format(new Date(milestone.deadline), 'MMM dd, yyyy')}
                  </div>
                </div>
              </div>

              <div className="flex items-center space-x-4">
                <div className="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    checked={milestone.completed}
                    disabled
                    className="w-5 h-5"
                  />
                  <span className="text-sm">Completed</span>
                </div>
                <div className="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    checked={milestone.approved}
                    disabled
                    className="w-5 h-5"
                  />
                  <span className="text-sm">Approved</span>
                </div>
              </div>

              <div className="mt-4 flex space-x-3">
                {isFreelancer && !milestone.completed && (
                  <button
                    onClick={() => completeMutation.mutate({ milestoneId: milestone.id })}
                    disabled={completeMutation.isPending}
                    className="px-4 py-2 bg-green-600 text-white rounded hover:bg-green-700 disabled:opacity-50"
                  >
                    Mark Complete
                  </button>
                )}

                {isClient && milestone.completed && !milestone.approved && (
                  <button
                    onClick={() => approveMutation.mutate({ milestoneId: milestone.id })}
                    disabled={approveMutation.isPending}
                    className="px-4 py-2 bg-blue-600 text-white rounded hover:bg-blue-700 disabled:opacity-50"
                  >
                    Approve & Release Funds
                  </button>
                )}

                {milestone.approved && (
                  <span className="px-4 py-2 bg-green-100 text-green-800 rounded">
                    ✓ Funds Released
                  </span>
                )}
              </div>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};
