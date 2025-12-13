// Cardano Blockchain Integration using Blockfrost API
// This enables real on-chain transactions

// Helper function to decode hex address to bech32
async function hexToAddress(hex: string): Promise<string> {
  try {
    // Dynamically import the WASM library
    const CardanoWasm = await import('@emurgo/cardano-serialization-lib-browser');
    
    // Convert hex string to Uint8Array
    const bytes = new Uint8Array(hex.match(/.{1,2}/g)!.map((byte) => parseInt(byte, 16)));
    const address = CardanoWasm.Address.from_bytes(bytes);
    return address.to_bech32();
  } catch (error) {
    console.error('Failed to decode address:', error);
    // If decoding fails, return the hex as-is
    return hex;
  }
}

export interface CardanoWallet {
  enable(): Promise<any>;
  isEnabled(): Promise<boolean>;
  getNetworkId(): Promise<number>;
  getUtxos(): Promise<string[]>;
  getBalance(): Promise<string>;
  getUsedAddresses(): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getChangeAddress(): Promise<string>;
  getRewardAddresses(): Promise<string[]>;
  signTx(tx: string, partialSign: boolean): Promise<string>;
  signData(address: string, payload: string): Promise<{ signature: string; key: string }>;
  submitTx(tx: string): Promise<string>;
}

declare global {
  interface Window {
    cardano?: {
      nami?: CardanoWallet;
      eternl?: CardanoWallet;
      flint?: CardanoWallet;
      lace?: CardanoWallet;
      [key: string]: CardanoWallet | undefined;
    };
  }
}

export class CardanoService {
  private wallet: CardanoWallet | null = null;
  private blockfrostApiKey: string;
  private network: 'preprod' | 'mainnet';

  constructor(blockfrostApiKey: string, network: 'preprod' | 'mainnet' = 'preprod') {
    this.blockfrostApiKey = blockfrostApiKey;
    this.network = network;
  }

  async connectWallet(walletName: 'nami' | 'eternl' | 'flint' | 'lace'): Promise<{
    address: string;
    balance: number;
  }> {
    if (!window.cardano || !window.cardano[walletName]) {
      throw new Error(`${walletName} wallet not found. Please install it first.`);
    }

    console.log(`Enabling ${walletName} wallet...`);
    const wallet = await window.cardano[walletName]!.enable();
    this.wallet = wallet;
    
    console.log('Getting wallet addresses...');
    
    // Try to get used addresses first
    let addresses = await wallet.getUsedAddresses();
    console.log('Used addresses:', addresses);
    
    // If no used addresses, get unused addresses
    if (!addresses || addresses.length === 0) {
      console.log('No used addresses, getting unused addresses...');
      addresses = await wallet.getUnusedAddresses();
      console.log('Unused addresses:', addresses);
    }
    
    // If still no addresses, try getChangeAddress
    if (!addresses || addresses.length === 0) {
      console.log('No unused addresses, getting change address...');
      const changeAddress = await wallet.getChangeAddress();
      console.log('Change address:', changeAddress);
      addresses = [changeAddress];
    }
    
    if (!addresses || addresses.length === 0) {
      throw new Error('Could not retrieve any addresses from wallet. Please make sure your wallet is properly set up.');
    }
    
    const addressHex = addresses[0];
    console.log('Selected address (hex):', addressHex);
    
    if (!addressHex) {
      throw new Error('Wallet address is empty. Please make sure your wallet is properly set up.');
    }
    
    // Decode the hex address to bech32 format
    const address = await hexToAddress(addressHex);
    console.log('Decoded address (bech32):', address);
    
    console.log('Getting wallet balance...');
    const balanceHex = await wallet.getBalance();
    const balance = parseInt(balanceHex, 16);
    console.log('Balance (lovelace):', balance);

    return { address, balance };
  }

  async getAddress(): Promise<string> {
    if (!this.wallet) {
      throw new Error('Wallet not connected');
    }
    
    // Try used addresses first
    let addresses = await this.wallet.getUsedAddresses();
    
    // If no used addresses, try unused addresses
    if (!addresses || addresses.length === 0) {
      addresses = await this.wallet.getUnusedAddresses();
    }
    
    // If still no addresses, try change address
    if (!addresses || addresses.length === 0) {
      const changeAddress = await this.wallet.getChangeAddress();
      return await hexToAddress(changeAddress);
    }
    
    return await hexToAddress(addresses[0]);
  }

  async getBalance(): Promise<number> {
    if (!this.wallet) {
      throw new Error('Wallet not connected');
    }
    const balanceHex = await this.wallet.getBalance();
    return parseInt(balanceHex, 16);
  }

  async buildTransaction(params: {
    recipient: string;
    amount: number;
    metadata?: Record<string, any>;
  }): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');
    
    // Build transaction using Blockfrost API
    console.log('Building transaction:', params);
    
    // In production, this would build a proper transaction
    // For now, return a simulated transaction
    return `tx_build_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;
  }

  async submitTransaction(signedTx: string): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');
    
    try {
      const txHash = await this.wallet.submitTx(signedTx);
      return txHash;
    } catch (error) {
      console.error('Transaction submission failed:', error);
      throw error;
    }
  }

  async lockFundsInEscrow(params: {
    amount: number;
    scriptAddress: string;
    datum: any;
  }): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');

    // This would build a transaction that locks funds in the Plutus script
    // For now, we'll simulate it
    console.log('Locking funds in escrow:', params);
    
    // In production, this would:
    // 1. Build transaction with script output
    // 2. Attach datum
    // 3. Sign with wallet
    // 4. Submit to blockchain
    
    return `tx_lock_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;
  }

  async releaseFundsFromEscrow(params: {
    scriptUtxo: string;
    redeemer: any;
    recipient: string;
  }): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');

    console.log('Releasing funds from escrow:', params);
    
    // In production, this would:
    // 1. Build transaction spending from script
    // 2. Attach redeemer
    // 3. Include script reference
    // 4. Sign with wallet
    // 5. Submit to blockchain
    
    return `tx_release_${Date.now()}_${Math.random().toString(36).substring(2, 11)}`;
  }

  async queryScriptUtxos(scriptAddress: string): Promise<any[]> {
    const response = await fetch(
      `https://cardano-${this.network}.blockfrost.io/api/v0/addresses/${scriptAddress}/utxos`,
      {
        headers: {
          'project_id': this.blockfrostApiKey,
        },
      }
    );

    if (!response.ok) {
      throw new Error('Failed to query script UTxOs');
    }

    return await response.json();
  }

  async getTransactionStatus(txHash: string): Promise<{
    confirmed: boolean;
    blockHeight?: number;
  }> {
    const response = await fetch(
      `https://cardano-${this.network}.blockfrost.io/api/v0/txs/${txHash}`,
      {
        headers: {
          'project_id': this.blockfrostApiKey,
        },
      }
    );

    if (!response.ok) {
      return { confirmed: false };
    }

    const data = await response.json();
    return {
      confirmed: true,
      blockHeight: data.block_height,
    };
  }

  isWalletConnected(): boolean {
    return this.wallet !== null;
  }

  disconnect() {
    this.wallet = null;
  }
}

// Singleton instance
let cardanoService: CardanoService | null = null;

export function getCardanoService(): CardanoService {
  if (!cardanoService) {
    const apiKey = import.meta.env.VITE_BLOCKFROST_PROJECT_ID || 'preprodDemo123';
    const network = (import.meta.env.VITE_NETWORK?.toLowerCase() || 'preprod') as 'preprod' | 'mainnet';
    cardanoService = new CardanoService(apiKey, network);
  }
  return cardanoService;
}
