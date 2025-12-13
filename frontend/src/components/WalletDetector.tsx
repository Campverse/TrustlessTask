import React, { useEffect, useState } from 'react';

export const WalletDetector: React.FC = () => {
  const [wallets, setWallets] = useState<string[]>([]);
  const [checking, setChecking] = useState(true);

  useEffect(() => {
    const detectWallets = async () => {
      console.log('🔍 Starting wallet detection...');
      
      // Check multiple times over 5 seconds
      const maxAttempts = 10;
      const delayBetweenAttempts = 500;
      
      for (let attempt = 1; attempt <= maxAttempts; attempt++) {
        console.log(`🔍 Detection attempt ${attempt}/${maxAttempts}`);
        
        const detected: string[] = [];
        
        if (typeof window !== 'undefined') {
          console.log('window.cardano exists:', !!window.cardano);
          
          if (window.cardano) {
            const keys = Object.keys(window.cardano);
            console.log('Keys in window.cardano:', keys);
            
            if (window.cardano.nami) detected.push('Nami');
            if (window.cardano.eternl) detected.push('Eternl');
            if (window.cardano.flint) detected.push('Flint');
            if (window.cardano.lace) detected.push('Lace');
            
            // Check for other wallets
            keys.forEach(key => {
              if (!['nami', 'eternl', 'flint', 'lace'].includes(key) && 
                  typeof window.cardano![key] === 'object') {
                detected.push(key.charAt(0).toUpperCase() + key.slice(1));
              }
            });
          }
        }
        
        if (detected.length > 0) {
          console.log('✅ Wallets found:', detected.join(', '));
          setWallets(detected);
          setChecking(false);
          return;
        }
        
        // Wait before next attempt
        if (attempt < maxAttempts) {
          await new Promise(resolve => setTimeout(resolve, delayBetweenAttempts));
        }
      }
      
      console.log('⚠️ No wallets detected after all attempts');
      setWallets([]);
      setChecking(false);
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
      <div className="fixed bottom-4 right-4 bg-red-100 text-red-800 px-4 py-3 rounded-lg shadow-lg text-sm max-w-md border-2 border-red-300">
        <div className="font-bold mb-2 text-base">🚫 No Cardano Wallet Found</div>
        <div className="text-xs space-y-2">
          <div className="font-semibold">Quick Fix:</div>
          <div className="bg-white p-2 rounded border border-red-200">
            1. Install <a href="https://www.lace.io/" target="_blank" rel="noopener noreferrer" className="underline font-bold text-blue-600">Lace Wallet</a>
            <br />
            2. Complete wallet setup
            <br />
            3. Refresh this page (F5)
            <br />
            4. Click "Connect Wallet"
          </div>
          <div className="mt-2 pt-2 border-t border-red-300">
            <div className="font-semibold mb-1">Troubleshooting:</div>
            <div>• Make sure extension is enabled</div>
            <div>• Check extension has site permissions</div>
            <div>• Try restarting your browser</div>
            <div>• Use Chrome, Edge, or Brave browser</div>
          </div>
          <div className="mt-2 text-center">
            <button
              onClick={() => window.location.reload()}
              className="px-3 py-1 bg-red-600 text-white rounded hover:bg-red-700 text-xs font-semibold"
            >
              Refresh Page
            </button>
          </div>
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
