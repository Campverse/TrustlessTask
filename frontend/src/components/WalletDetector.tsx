import React, { useEffect, useState } from 'react';

export const WalletDetector: React.FC = () => {
  const [wallets, setWallets] = useState<string[]>([]);
  const [checking, setChecking] = useState(true);

  useEffect(() => {
    const detectWallets = async () => {
      // Wait for wallets to inject (some take longer)
      await new Promise(resolve => setTimeout(resolve, 1500));
      
      const detected: string[] = [];
      
      if (typeof window !== 'undefined' && window.cardano) {
        console.log('🔍 Checking for wallets...');
        console.log('Available in window.cardano:', Object.keys(window.cardano));
        
        if (window.cardano.nami) detected.push('Nami');
        if (window.cardano.eternl) detected.push('Eternl');
        if (window.cardano.flint) detected.push('Flint');
        if (window.cardano.lace) detected.push('Lace');
        
        // Check for other wallets
        Object.keys(window.cardano).forEach(key => {
          if (!['nami', 'eternl', 'flint', 'lace'].includes(key) && 
              typeof window.cardano![key] === 'object') {
            detected.push(key.charAt(0).toUpperCase() + key.slice(1));
          }
        });
      }
      
      setWallets(detected);
      setChecking(false);
      
      console.log('🔍 Wallet detection complete');
      console.log('✅ Detected wallets:', detected.length > 0 ? detected.join(', ') : 'None');
    };

    detectWallets();
  }, []);

  if (checking) {
    return (
      <div className="fixed bottom-4 right-4 bg-blue-100 text-blue-800 px-4 py-2 rounded-lg shadow-lg text-sm">
        🔍 Detecting wallets...
      </div>
    );
  }

  if (wallets.length === 0) {
    return (
      <div className="fixed bottom-4 right-4 bg-yellow-100 text-yellow-800 px-4 py-3 rounded-lg shadow-lg text-sm max-w-sm">
        <div className="font-semibold mb-1">⚠️ No Cardano Wallet Detected</div>
        <div className="text-xs">
          Using demo mode. Install{' '}
          <a 
            href="https://namiwallet.io/" 
            target="_blank" 
            rel="noopener noreferrer"
            className="underline font-semibold"
          >
            Nami
          </a>
          {' '}for real blockchain.
        </div>
      </div>
    );
  }

  return (
    <div className="fixed bottom-4 right-4 bg-green-100 text-green-800 px-4 py-3 rounded-lg shadow-lg text-sm">
      <div className="font-semibold mb-1">✅ Wallets Detected</div>
      <div className="text-xs">{wallets.join(', ')}</div>
    </div>
  );
};
