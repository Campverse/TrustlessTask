import { useState, useEffect } from 'react';
import type { WalletState } from '../types';
import { getCardanoService } from '../services/cardano';

export const useWallet = () => {
  const [wallet, setWallet] = useState<WalletState>({
    connected: false,
    address: null,
    balance: 0,
    lucid: null,
    walletName: undefined,
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
      // Special handling for Nami - check if it's available through Lace
      let actualWalletName = walletName;
      
      if (walletName === 'nami') {
        console.log('🔍 Checking for Nami wallet...');
        
        // Check if Nami is available directly
        if (window.cardano?.nami) {
          console.log('✅ Found Nami wallet directly');
          actualWalletName = 'nami';
        }
        // Check if Lace is available (Lace includes Nami compatibility)
        else if (window.cardano?.lace) {
          console.log('✅ Found Lace wallet (includes Nami compatibility)');
          console.log('💡 Using Lace wallet to access Nami address');
          actualWalletName = 'lace';
        }
        else {
          throw new Error(
            `Nami wallet not found.\n\n` +
            `Please install either:\n` +
            `• Nami: https://namiwallet.io/\n` +
            `• Lace: https://www.lace.io/ (includes Nami compatibility)\n\n` +
            `After installation, refresh this page and try again.`
          );
        }
      }
      // For other wallets, check normally
      else {
        console.log('🔍 Checking for', walletName, 'wallet...');
        console.log('window.cardano exists:', !!window.cardano);
        console.log('Available wallets:', window.cardano ? Object.keys(window.cardano) : 'none');
        console.log(`window.cardano.${walletName} exists:`, !!(window.cardano && window.cardano[walletName]));
        
        if (typeof window === 'undefined' || !window.cardano || !window.cardano[walletName]) {
          throw new Error(
            `${walletName} wallet not found.\n\n` +
            `Please install the ${walletName} browser extension:\n` +
            `• Lace: https://www.lace.io/\n` +
            `• Eternl: https://eternl.io/\n` +
            `• Flint: https://flint-wallet.com/\n\n` +
            `Available wallets: ${window.cardano ? Object.keys(window.cardano).join(', ') : 'none'}\n\n` +
            `After installation, refresh this page and try again.`
          );
        }
      }

      console.log(`✅ ${actualWalletName} wallet detected! Connecting...`);
      
      // Connect to the real Cardano wallet (use actualWalletName which might be 'lace' for 'nami')
      const cardanoService = getCardanoService();
      const { address, balance } = await cardanoService.connectWallet(actualWalletName);

      if (!address) {
        throw new Error('Failed to get wallet address. Please try again.');
      }

      const newWalletState = {
        connected: true,
        address,
        balance,
        lucid: cardanoService as any,
        walletName: actualWalletName,
      };
      
      setWallet(newWalletState);
      localStorage.setItem('connectedWallet', walletName);
      
      console.log('✅ Wallet connected successfully');
      console.log('📍 Wallet Type:', actualWalletName.toUpperCase());
      console.log('📍 Address:', address);
      console.log('💰 Balance:', (balance / 1_000_000).toFixed(2), 'ADA');
      
      const walletInfo = walletName === 'nami' && actualWalletName === 'lace' 
        ? `\n\n⚠️ Note: Using Lace wallet (Nami not found)\nThis is a different wallet than standalone Nami.\nTo use your Nami address, install Nami extension or import your Nami seed phrase into Lace.`
        : '';
      
      alert(`✅ Wallet connected successfully!\n\nWallet: ${actualWalletName.toUpperCase()}\nAddress: ${address.slice(0, 20)}...\nBalance: ${(balance / 1_000_000).toFixed(2)} ADA${walletInfo}`);
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
      walletName: undefined,
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
