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

  if (!wallet.connected || !wallet.address) {
    return (
      <div className="text-center py-6 px-4 sm:py-12">
        <div className="bg-blue-50 border border-blue-200 rounded-lg p-4 sm:p-8 max-w-lg mx-auto">
          <div className="text-4xl sm:text-6xl mb-4">🔐</div>
          <h2 className="text-xl sm:text-2xl font-bold text-gray-800 mb-3">Connect Your Cardano Wallet</h2>
          <p className="text-sm sm:text-base text-gray-700 mb-6">
            You need to connect a Cardano wallet to create projects on TrustlessTask.
          </p>
          
          <div className="bg-white rounded-lg p-4 sm:p-6 mb-6 text-left">
            <h3 className="font-semibold text-gray-800 mb-3 text-sm sm:text-base">Supported Wallets:</h3>
            <ul className="space-y-2 text-xs sm:text-sm text-gray-600">
              <li>• <strong>Nami</strong> - https://namiwallet.io/</li>
              <li>• <strong>Lace</strong> - https://www.lace.io/</li>
              <li>• <strong>Eternl</strong> - https://eternl.io/</li>
              <li>• <strong>Flint</strong> - https://flint-wallet.com/</li>
            </ul>
          </div>

          <div className="bg-yellow-50 border border-yellow-200 rounded-lg p-3 sm:p-4 mb-6 text-left">
            <h3 className="font-semibold text-yellow-800 mb-2 text-sm sm:text-base">📝 Steps:</h3>
            <ol className="text-xs sm:text-sm text-gray-700 space-y-1 list-decimal list-inside">
              <li>Install a Cardano wallet browser extension</li>
              <li>Create or import your wallet</li>
              <li>Click "Connect Wallet" in the top right corner</li>
              <li>Select your wallet and approve the connection</li>
              <li>Return here to create your project</li>
            </ol>
          </div>

          <button
            onClick={() => window.location.reload()}
            className="w-full sm:w-auto px-6 py-3 bg-blue-600 text-white rounded-lg hover:bg-blue-700 font-medium text-sm sm:text-base"
          >
            Refresh Page After Installing Wallet
          </button>
        </div>
      </div>
    );
  }

  return (
    <div className="max-w-3xl mx-auto px-4 sm:px-6">
      <h1 className="text-2xl sm:text-3xl font-bold mb-6 sm:mb-8">Create New Project</h1>
      
      <div className="bg-blue-50 border border-blue-200 rounded-lg p-3 sm:p-4 mb-4 sm:mb-6">
        <p className="text-xs sm:text-sm text-blue-800 break-all">
          <strong>Your Address:</strong> <span className="font-mono">{wallet.address}</span>
        </p>
      </div>
      
      <div className="bg-white rounded-lg shadow-md p-4 sm:p-8">
        <CreateProjectForm
          clientAddress={wallet.address}
          onSubmit={(data: CreateProjectRequest) => {
            console.log('📤 Submitting project with data:', JSON.stringify(data, null, 2));
            createMutation.mutate(data);
          }}
        />
      </div>

      {createMutation.isPending && (
        <div className="mt-4 text-center text-sm sm:text-base text-gray-600 px-4">
          Creating project and submitting transaction...
        </div>
      )}

      {createMutation.isError && (
        <div className="mt-4 p-3 sm:p-4 bg-red-100 text-red-700 rounded-lg mx-4 sm:mx-0">
          <div className="font-semibold mb-2 text-sm sm:text-base">Error creating project</div>
          <div className="text-xs sm:text-sm mb-2">
            {createMutation.error instanceof Error 
              ? createMutation.error.message 
              : 'Unknown error occurred'}
          </div>
          {(createMutation.error as any)?.response?.data?.validationErrors && (
            <div className="mt-2 text-xs">
              <div className="font-semibold mb-1">Validation errors:</div>
              <ul className="list-disc list-inside">
                {(createMutation.error as any).response.data.validationErrors.map((err: any, i: number) => (
                  <li key={i}>
                    <strong>{err.field}:</strong> {err.message} (received: {err.received})
                  </li>
                ))}
              </ul>
            </div>
          )}
          <div className="text-xs mt-2 opacity-75">
            Check browser console (F12) for more details.
          </div>
        </div>
      )}

      {createMutation.isSuccess && (
        <div className="mt-4 p-3 sm:p-4 bg-green-100 text-green-700 rounded-lg mx-4 sm:mx-0 text-sm sm:text-base">
          ✅ Project created successfully! Redirecting...
        </div>
      )}
    </div>
  );
};
