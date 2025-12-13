import React from 'react';
import { useQuery } from '@tanstack/react-query';
import { usersApi } from '../services/api';
import type { WalletState } from '../types';

interface ProfilePageProps {
  wallet: WalletState;
}

export const ProfilePage: React.FC<ProfilePageProps> = ({ wallet }) => {
  const { data: profile, isLoading, isError, error } = useQuery({
    queryKey: ['profile', wallet.address],
    queryFn: () => usersApi.getProfile(wallet.address!),
    enabled: !!wallet.address,
    retry: 2,
  });

  if (!wallet.connected) {
    return (
      <div className="text-center py-12">
        <p className="text-gray-600">Please connect your wallet to view profile</p>
      </div>
    );
  }

  if (isLoading) {
    return (
      <div className="text-center py-12">
        <div className="inline-block animate-spin rounded-full h-12 w-12 border-b-2 border-blue-600"></div>
        <p className="mt-4 text-gray-600">Loading profile...</p>
      </div>
    );
  }

  if (isError) {
    return (
      <div className="text-center py-12">
        <div className="bg-red-100 text-red-700 p-6 rounded-lg max-w-md mx-auto">
          <h2 className="text-xl font-semibold mb-2">Failed to load profile</h2>
          <p className="text-sm">{error instanceof Error ? error.message : 'Unknown error'}</p>
        </div>
      </div>
    );
  }

  return (
    <div className="max-w-4xl mx-auto">
      <h1 className="text-3xl font-bold mb-8">Your Profile</h1>

      <div className="bg-white rounded-lg shadow-md p-8 mb-6">
        <div className="mb-6">
          <div className="text-sm text-gray-500 mb-1">Wallet Address</div>
          <div className="font-mono text-sm break-all">{profile?.address}</div>
        </div>

        <div className="grid grid-cols-2 md:grid-cols-4 gap-6">
          <div className="text-center p-4 bg-blue-50 rounded-lg">
            <div className="text-3xl font-bold text-blue-600">
              {profile?.completedProjects || 0}
            </div>
            <div className="text-sm text-gray-600 mt-1">Completed Projects</div>
          </div>

          <div className="text-center p-4 bg-green-50 rounded-lg">
            <div className="text-3xl font-bold text-green-600">
              {((profile?.totalEarned || 0) / 1_000_000).toFixed(2)} ₳
            </div>
            <div className="text-sm text-gray-600 mt-1">Total Earned</div>
          </div>

          <div className="text-center p-4 bg-yellow-50 rounded-lg">
            <div className="text-3xl font-bold text-yellow-600">
              {profile?.averageRating.toFixed(2) || '0.00'}
            </div>
            <div className="text-sm text-gray-600 mt-1">Average Rating</div>
          </div>

          <div className="text-center p-4 bg-red-50 rounded-lg">
            <div className="text-3xl font-bold text-red-600">
              {profile?.disputes || 0}
            </div>
            <div className="text-sm text-gray-600 mt-1">Disputes</div>
          </div>
        </div>
      </div>

      <div className="bg-white rounded-lg shadow-md p-8">
        <h2 className="text-2xl font-bold mb-4">Reputation Score</h2>
        <div className="flex items-center space-x-4">
          <div className="flex-1 bg-gray-200 rounded-full h-4">
            <div
              className="bg-blue-600 h-4 rounded-full"
              style={{ width: `${(profile?.averageRating || 0) * 20}%` }}
            />
          </div>
          <span className="text-lg font-semibold">
            {profile?.averageRating.toFixed(1) || '0.0'} / 5.0
          </span>
        </div>
      </div>
    </div>
  );
};
