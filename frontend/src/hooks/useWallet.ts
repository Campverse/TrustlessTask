import { useState, useEffect } from 'react';
import type { WalletState } from '../types';
import { getCardanoService } from '../services/cardano';

const USE_REAL_BLOCKCHAIN = true; // Set to true when you have a Cardano wallet installed

export const useWallet = () => {
  const [wallet, setWallet] = useState<WalletState>({
    connected: false,
    address: null,
    balance: 0,
    lucid: null,
  });

  const connectWallet = async (walletName: 'nami' | 'eternl' | 'flint' | 'lace') => {
    try {
      // Wait for wallet extension to load (they inject into window.cardano)
      const checkWallet = async (): Promise<boolean> => {
        if (typeof window === 'undefined') return false;
        
        // Wait up to 3 seconds for wallet to be available
        for (let i = 0; i < 30; i++) {
          if (window.cardano && window.cardano[walletName]) {
            return true;
          }
          await new Promise(resolve => setTimeout(resolve, 100));
        }
        return false;
      };

      const hasWallet = await checkWallet();

      if (USE_REAL_BLOCKCHAIN && hasWallet) {
        // Real Cardano blockchain connection
        console.log(`✅ ${walletName} wallet detected! Connecting...`);
        const cardanoService = getCardanoService();
        const { address, balance } = await cardanoService.connectWallet(walletName);

        setWallet({
          connected: true,
          address,
          balance,
          lucid: cardanoService as any,
        });

        localStorage.setItem('connectedWallet', walletName);
        console.log('✅ Wallet connected:', address);
        console.log('💰 Balance:', balance / 1_000_000, 'ADA');
        return;
      }

      // Demo mode - simulate wallet connection
      console.log(`🎮 Demo mode: Simulating ${walletName} wallet connection...`);
      await new Promise(resolve => setTimeout(resolve, 500));
      
      setWallet({
        connected: true,
        address: 'addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp',
        balance: 1000000000, // 1000 ADA
        lucid: null,
      });
      
      localStorage.setItem('connectedWallet', walletName);
      console.log('✅ Demo wallet connected');
      
      if (USE_REAL_BLOCKCHAIN && !hasWallet) {
        console.warn(`⚠️ ${walletName} wallet not found after 3 seconds. Using demo mode.`);
        console.log('Available wallets:', window.cardano ? Object.keys(window.cardano) : 'none');
        alert(`${walletName} wallet not detected. Using demo mode.\n\nTo use real wallet:\n1. Install ${walletName} extension from Chrome Web Store\n2. Refresh this page\n3. Make sure wallet is unlocked\n4. Try connecting again`);
      }
    } catch (error) {
      console.error('❌ Failed to connect wallet:', error);
      alert(`Failed to connect ${walletName} wallet.\n\nError: ${error instanceof Error ? error.message : 'Unknown error'}\n\nFalling back to demo mode...`);
      
      // Fallback to demo mode on error
      setWallet({
        connected: true,
        address: 'addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp',
        balance: 1000000000,
        lucid: null,
      });
      localStorage.setItem('connectedWallet', walletName);
    }
  };

  const disconnectWallet = () => {
    setWallet({
      connected: false,
      address: null,
      balance: 0,
      lucid: null,
    });
    localStorage.removeItem('connectedWallet');
  };

  useEffect(() => {
    const savedWallet = localStorage.getItem('connectedWallet');
    if (savedWallet) {
      connectWallet(savedWallet as any).catch(console.error);
    }
  }, []);

  return { wallet, connectWallet, disconnectWallet };
};
