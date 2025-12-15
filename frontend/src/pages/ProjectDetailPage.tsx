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

  const { data: project, isLoading } = useQuery({
    queryKey: ['project', id],
    queryFn: () => projectsApi.getById(id!),
    enabled: !!id,
  });

  const completeMutation = useMutation({
    mutationFn: async ({ milestoneId }: { milestoneId: number }) => {
      if (!project) throw new Error('Project not found');
      
      const milestone = project.milestones.find(m => m.id === milestoneId);
      if (!milestone) throw new Error('Milestone not found');
      
      console.log('📝 Marking milestone as complete with blockchain transaction...');
      console.log('Milestone:', milestone.description);
      console.log('Client:', project.clientAddress);
      
      // Import Cardano service dynamically
      const { getCardanoService } = await import('../services/cardano');
      const cardanoService = getCardanoService();
      
      if (!cardanoService.isWalletConnected()) {
        throw new Error('❌ Wallet not connected.\n\nPlease connect your Cardano wallet first to mark milestone as complete.');
      }
      
      // Build and sign the completion transaction
      console.log('🔨 Building completion transaction (1 ADA to client as proof)...');
      const signedTx = await cardanoService.buildMilestoneCompletion({
        projectId: id!,
        milestoneId,
        clientAddress: project.clientAddress,
        projectTitle: project.title,
        milestoneDescription: milestone.description,
      });
      
      console.log('✅ Transaction signed, submitting to blockchain...');
      
      // Submit the transaction to Cardano blockchain
      const txHash = await cardanoService.submitTransaction(signedTx);
      
      console.log('✅ Completion transaction submitted to Cardano blockchain!');
      console.log('Transaction hash:', txHash);
      console.log(`View on explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
      
      // Update backend with real transaction hash
      return projectsApi.completeMilestone(id!, milestoneId, txHash);
    },
    onSuccess: (data: any) => {
      console.log('✅ Milestone marked as complete on blockchain');
      const txHash = data.txHash || 'unknown';
      alert(
        `✅ Milestone Marked Complete!\n\n` +
        `Transaction Hash: ${txHash}\n\n` +
        `View on explorer:\n` +
        `https://preprod.cardanoscan.io/transaction/${txHash}\n\n` +
        `A proof-of-completion transaction (1 ADA) has been sent to the client.\n` +
        `The client can now approve and release the full milestone payment.`
      );
      queryClient.invalidateQueries({ queryKey: ['project', id] });
    },
    onError: (error: any) => {
      console.error('❌ Failed to complete milestone:', error);
      alert(
        `❌ Failed to Mark Complete\n\n` +
        `${error.message || 'Failed to complete milestone'}\n\n` +
        `Please check:\n` +
        `• Wallet is connected\n` +
        `• You have at least 1.2 ADA (1 ADA + fees)\n` +
        `• Blockfrost API key is configured\n` +
        `• You are running in development mode (npm run dev)`
      );
    },
  });

  const approveMutation = useMutation({
    mutationFn: async ({ milestoneId }: { milestoneId: number }) => {
      if (!project) throw new Error('Project not found');
      
      const milestone = project.milestones.find(m => m.id === milestoneId);
      if (!milestone) throw new Error('Milestone not found');
      
      console.log('💰 Approving milestone and releasing funds...');
      console.log('Amount:', milestone.amount / 1_000_000, 'ADA');
      console.log('Recipient:', project.freelancerAddress);
      
      // Import Cardano service dynamically
      const { getCardanoService } = await import('../services/cardano');
      const cardanoService = getCardanoService();
      
      if (!cardanoService.isWalletConnected()) {
        throw new Error('❌ Wallet not connected.\n\nPlease connect your Cardano wallet first to approve and release funds.');
      }
      
      // Build and sign the transaction
      console.log('🔨 Building real blockchain transaction...');
      const signedTx = await cardanoService.buildMilestonePayment({
        projectId: id!,
        milestoneId,
        recipient: project.freelancerAddress,
        amount: milestone.amount,
        projectTitle: project.title,
        milestoneDescription: milestone.description,
      });
      
      console.log('✅ Transaction signed, submitting to blockchain...');
      
      // Submit the transaction to Cardano blockchain
      const txHash = await cardanoService.submitTransaction(signedTx);
      
      console.log('✅ Transaction submitted to Cardano blockchain!');
      console.log('Transaction hash:', txHash);
      console.log(`View on explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
      
      // Update backend with real transaction hash
      return projectsApi.approveMilestone(id!, milestoneId, txHash);
    },
    onSuccess: (data: any) => {
      console.log('✅ Milestone approved and funds released on blockchain');
      const txHash = data.txHash || 'unknown';
      alert(
        `✅ Funds Released Successfully!\n\n` +
        `Transaction Hash: ${txHash}\n\n` +
        `View on explorer:\n` +
        `https://preprod.cardanoscan.io/transaction/${txHash}\n\n` +
        `The freelancer will receive the funds shortly.`
      );
      queryClient.invalidateQueries({ queryKey: ['project', id] });
    },
    onError: (error: any) => {
      console.error('❌ Failed to approve milestone:', error);
      alert(
        `❌ Transaction Failed\n\n` +
        `${error.message || 'Failed to approve milestone'}\n\n` +
        `Please check:\n` +
        `• Wallet is connected\n` +
        `• You have sufficient ADA (amount + ~0.17 ADA fee)\n` +
        `• Blockfrost API key is configured\n` +
        `• You are running in development mode (npm run dev)`
      );
    },
  });

  if (isLoading) {
    return <div className="text-center py-12">Loading project...</div>;
  }

  if (!project) {
    return <div className="text-center py-12">Project not found</div>;
  }

  const isClient = wallet.address && wallet.address === project.clientAddress;
  const isFreelancer = wallet.address && wallet.address === project.freelancerAddress;

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
                    readOnly
                  />
                  <span className="text-sm">Completed</span>
                </div>
                <div className="flex items-center space-x-2">
                  <input
                    type="checkbox"
                    checked={milestone.approved}
                    disabled
                    className="w-5 h-5"
                    readOnly
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
