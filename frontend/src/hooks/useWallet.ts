import { useState, useEffect } from 'react';
import type { WalletState } from '../types';
import { getCardanoService } from '../services/cardano';

export const useWallet = () => {
  const [wallet, setWallet] = useState<WalletState>({
    connected: false,
    address: null,
    balance: 0,
    lucid: null,
  });

  // Debug: Log wallet state changes
  useEffect(() => {
    console.log('💼 Wallet state updated:', {
      connected: wallet.connected,
      address: wallet.address ? `${wallet.address.slice(0, 20)}...` : 'null',
      balance: wallet.balance,
    });
  }, [wallet]);

  const connectWallet = async (walletName: 'nami' | 'eternl' | 'flint' | 'lace') => {
    console.log(`🔌 Connecting to ${walletName} wallet...`);
    
    try {
      // Check if wallet extension is available
      if (typeof window === 'undefined' || !window.cardano || !window.cardano[walletName]) {
        throw new Error(
          `${walletName} wallet not found.\n\n` +
          `Please install the ${walletName} browser extension:\n` +
          `• Nami: https://namiwallet.io/\n` +
          `• Lace: https://www.lace.io/\n` +
          `• Eternl: https://eternl.io/\n` +
          `• Flint: https://flint-wallet.com/\n\n` +
          `After installation, refresh this page and try again.`
        );
      }

      console.log(`✅ ${walletName} wallet detected! Connecting...`);
      
      // Connect to the real Cardano wallet
      const cardanoService = getCardanoService();
      const { address, balance } = await cardanoService.connectWallet(walletName);

      if (!address) {
        throw new Error('Failed to get wallet address. Please try again.');
      }

      const newWalletState = {
        connected: true,
        address,
        balance,
        lucid: cardanoService as any,
      };
      
      setWallet(newWalletState);
      localStorage.setItem('connectedWallet', walletName);
      
      console.log('✅ Wallet connected successfully');
      console.log('📍 Address:', address);
      console.log('💰 Balance:', (balance / 1_000_000).toFixed(2), 'ADA');
      
      alert(`✅ Wallet connected successfully!\n\nAddress: ${address.slice(0, 20)}...\nBalance: ${(balance / 1_000_000).toFixed(2)} ADA`);
    } catch (error) {
      console.error('❌ Failed to connect wallet:', error);
      
      const errorMessage = error instanceof Error ? error.message : 'Unknown error occurred';
      
      alert(`❌ Failed to connect ${walletName} wallet\n\n${errorMessage}`);
      
      // Don't set any wallet state on error
      throw error;
    }
  };

  const disconnectWallet = () => {
    console.log('🔌 Disconnecting wallet...');
    setWallet({
      connected: false,
      address: null,
      balance: 0,
      lucid: null,
    });
    localStorage.removeItem('connectedWallet');
    console.log('✅ Wallet disconnected');
  };

  // Auto-reconnect on mount if wallet was previously connected
  useEffect(() => {
    const savedWallet = localStorage.getItem('connectedWallet');
    if (savedWallet) {
      console.log('🔄 Auto-reconnecting to saved wallet:', savedWallet);
      
      // Attempt to reconnect
      connectWallet(savedWallet as any).catch((error) => {
        console.error('❌ Failed to auto-reconnect:', error);
        // Clear saved wallet if auto-reconnect fails
        localStorage.removeItem('connectedWallet');
      });
    }
  }, []);

  return { wallet, connectWallet, disconnectWallet };
};
