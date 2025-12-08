import { useState, useEffect } from 'react';
import type { WalletState } from '../types';

const DEMO_MODE = true; // Keep true for demo (real Cardano wallets require blockchain connection)

export const useWallet = () => {
  const [wallet, setWallet] = useState<WalletState>({
    connected: false,
    address: null,
    balance: 0,
    lucid: null,
  });

  const connectWallet = async (walletName: 'nami' | 'eternl' | 'flint') => {
    try {
      if (DEMO_MODE) {
        // Demo mode - simulate wallet connection
        await new Promise(resolve => setTimeout(resolve, 500));
        setWallet({
          connected: true,
          address: 'addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp',
          balance: 1000000000, // 1000 ADA
          lucid: null,
        });
        localStorage.setItem('connectedWallet', walletName);
        return;
      }

      // Real wallet connection (requires lucid-cardano)
      // Uncomment when using real wallets:
      /*
      const { Lucid, Blockfrost } = await import('lucid-cardano');
      const lucid = await Lucid.new(
        new Blockfrost(
          import.meta.env.VITE_BLOCKFROST_URL,
          import.meta.env.VITE_BLOCKFROST_PROJECT_ID
        ),
        import.meta.env.VITE_NETWORK || 'Preprod'
      );

      const api = await window.cardano[walletName].enable();
      lucid.selectWallet(api);

      const address = await lucid.wallet.address();
      const utxos = await lucid.wallet.getUtxos();
      const balance = utxos.reduce((acc, utxo) => acc + Number(utxo.assets.lovelace), 0);

      setWallet({
        connected: true,
        address,
        balance,
        lucid,
      });

      localStorage.setItem('connectedWallet', walletName);
      */
    } catch (error) {
      console.error('Failed to connect wallet:', error);
      throw error;
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
