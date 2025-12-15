// Cardano Blockchain Integration using Blockfrost API
// This enables real on-chain transactions

import { bech32 } from 'bech32';

// Convert hex address to bech32 format
function hexToAddress(hex: string): string {
  try {
    console.log('🔄 Converting address from hex:', hex);
    
    // Check if it's already a bech32 address
    if (hex.startsWith('addr') || hex.startsWith('stake')) {
      console.log('✅ Already in bech32 format');
      return hex;
    }
    
    // Convert hex string to byte array
    const hexMatch = hex.match(/.{1,2}/g);
    if (!hexMatch) {
      throw new Error('Invalid hex string');
    }
    
    const bytes = hexMatch.map(byte => parseInt(byte, 16));
    console.log('📊 Byte array length:', bytes.length);
    console.log('📊 First 4 bytes:', bytes.slice(0, 4).map(b => b.toString(16).padStart(2, '0')).join(' '));
    
    // Determine prefix based on first byte (network tag)
    // Cardano addresses: first 4 bits indicate network
    // 0000 = testnet, 0001 = mainnet
    const firstByte = bytes[0];
    const networkId = (firstByte >> 4) & 0x0F;
    const prefix = networkId === 0 ? 'addr_test' : 'addr';
    
    console.log('🌐 Network ID:', networkId, 'Prefix:', prefix);
    console.log('🔢 First byte:', firstByte.toString(16).padStart(2, '0'));
    
    // Convert to Uint8Array for bech32 library
    const uint8Array = new Uint8Array(bytes);
    
    // Convert to 5-bit words for bech32 encoding
    const words = bech32.toWords(uint8Array);
    console.log('📝 Words length:', words.length);
    
    // Encode to bech32 with proper limit (1023 is max for bech32)
    // Use bech32.encode with limit parameter
    const address = bech32.encode(prefix, words, 1023);
    
    console.log('✅ Converted to bech32:', address);
    console.log('📏 Address length:', address.length);
    
    // Verify the conversion by decoding it back
    try {
      const decoded = bech32.decode(address, 1023);
      console.log('✓ Verification: decoded prefix =', decoded.prefix);
    } catch (e) {
      console.warn('⚠️ Could not verify decoded address:', e);
    }
    
    return address;
  } catch (error) {
    console.error('❌ Failed to convert address:', error);
    console.error('Hex was:', hex);
    // Return hex as fallback
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

export interface CardanoWallets {
  nami?: CardanoWallet;
  eternl?: CardanoWallet;
  flint?: CardanoWallet;
  lace?: CardanoWallet;
  [key: string]: CardanoWallet | undefined;
}

declare global {
  interface Window {
    cardano?: CardanoWallets;
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
    
    // Convert the hex address to readable format
    const address = hexToAddress(addressHex);
    console.log('Converted address:', address);
    
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
      return hexToAddress(changeAddress);
    }
    
    return hexToAddress(addresses[0]);
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
    
    console.log('🔨 Building real Cardano transaction with Lucid...');
    console.log('Recipient:', params.recipient);
    console.log('Amount:', params.amount / 1_000_000, 'ADA');
    
    try {
      // Import Lucid dynamically
      const { Lucid, Blockfrost } = await import('lucid-cardano');
      
      // Get sender address
      const senderAddress = await this.getAddress();
      console.log('Sender:', senderAddress);
      
      // Check wallet balance
      const balance = await this.getBalance();
      console.log('Wallet balance:', balance / 1_000_000, 'ADA');
      
      if (balance === 0) {
        throw new Error(
          'No funds available in wallet.\n\n' +
          'To make real blockchain transactions, you need testnet ADA:\n\n' +
          '1. Visit: https://docs.cardano.org/cardano-testnet/tools/faucet/\n' +
          '2. Enter your wallet address: ' + senderAddress + '\n' +
          '3. Request testnet ADA (free)\n' +
          '4. Wait ~20 seconds for confirmation\n' +
          '5. Refresh and try again'
        );
      }
      
      if (balance < params.amount + 2_000_000) {
        throw new Error(
          `Insufficient funds.\n\n` +
          `Required: ${(params.amount + 2_000_000) / 1_000_000} ADA (including fees)\n` +
          `Available: ${balance / 1_000_000} ADA\n\n` +
          `Get more testnet ADA from the faucet.`
        );
      }
      
      // Check for valid Blockfrost API key
      const blockfrostApiKey = import.meta.env.VITE_BLOCKFROST_PROJECT_ID;
      
      if (!blockfrostApiKey || blockfrostApiKey === 'preprodDemo123') {
        throw new Error(
          'Valid Blockfrost API key required for real transactions.\n\n' +
          'Setup instructions:\n' +
          '1. Get FREE API key from https://blockfrost.io\n' +
          '2. Create account and new project (Preprod Testnet)\n' +
          '3. Copy your project ID\n' +
          '4. Create frontend/.env file:\n' +
          '   VITE_BLOCKFROST_PROJECT_ID=preprodYourKeyHere\n' +
          '5. Restart frontend server: npm run dev\n\n' +
          'Without a valid API key, real blockchain transactions cannot be made.'
        );
      }
      
      console.log('Initializing Lucid with Blockfrost...');
      
      // Initialize Lucid with Blockfrost
      const lucid = await Lucid.new(
        new Blockfrost('https://cardano-preprod.blockfrost.io/api/v0', blockfrostApiKey),
        'Preprod'
      );
      
      // Select wallet
      lucid.selectWallet(this.wallet);
      
      console.log('✅ Lucid initialized, building transaction...');
      
      // Build transaction
      const tx = await lucid
        .newTx()
        .payToAddress(params.recipient, { lovelace: BigInt(params.amount) })
        .attachMetadata(674, params.metadata || {})
        .complete();
      
      console.log('Transaction built, requesting wallet signature...');
      
      // Sign transaction
      const signedTx = await tx.sign().complete();
      
      console.log('✅ Transaction signed successfully');
      
      // Return signed transaction as hex
      return signedTx.toString();
    } catch (error: any) {
      console.error('❌ Failed to build transaction:', error);
      
      if (error.message?.includes('Invalid project token')) {
        throw new Error(
          'Blockfrost API key is invalid.\n\n' +
          'To enable real transactions:\n' +
          '1. Get free API key from https://blockfrost.io\n' +
          '2. Add to frontend/.env:\n' +
          '   VITE_BLOCKFROST_PROJECT_ID=preprodYourKeyHere\n' +
          '3. Restart frontend server'
        );
      }
      
      throw error;
    }
  }

  async submitTransaction(signedTx: string): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');
    
    try {
      console.log('📤 Submitting transaction to Cardano blockchain...');
      
      // Check for valid Blockfrost API key
      const blockfrostApiKey = import.meta.env.VITE_BLOCKFROST_PROJECT_ID;
      
      if (!blockfrostApiKey || blockfrostApiKey === 'preprodDemo123') {
        throw new Error('Valid Blockfrost API key required. See setup instructions.');
      }
      
      // Import Lucid dynamically
      const { Lucid, Blockfrost } = await import('lucid-cardano');
      
      // Initialize Lucid with Blockfrost
      const lucid = await Lucid.new(
        new Blockfrost('https://cardano-preprod.blockfrost.io/api/v0', blockfrostApiKey),
        'Preprod'
      );
      
      // Submit the signed transaction
      const txHash = await lucid.provider.submitTx(signedTx);
      
      console.log('✅ Transaction submitted successfully!');
      console.log('Transaction hash:', txHash);
      console.log(`View on explorer: https://preprod.cardanoscan.io/transaction/${txHash}`);
      
      return txHash;
    } catch (error: any) {
      console.error('❌ Transaction submission failed:', error);
      throw new Error(`Failed to submit transaction: ${error.message}`);
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
