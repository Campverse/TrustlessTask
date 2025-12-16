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

export class CardanoService {
  private wallet: CardanoWalletApi | null = null;
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
    
    console.log('🔨 Building transaction using Cardano Serialization Library...');
    console.log('Recipient:', params.recipient);
    console.log('Amount:', params.amount / 1_000_000, 'ADA');
    
    try {
      // Import Cardano serialization library
      const CSL = await import('@emurgo/cardano-serialization-lib-browser');
      
      // Get protocol parameters from Blockfrost
      const blockfrostApiKey = import.meta.env.VITE_BLOCKFROST_PROJECT_ID;
      
      if (!blockfrostApiKey || blockfrostApiKey === 'preprodDemo123') {
        throw new Error(
          'Valid Blockfrost API key required.\n\n' +
          'Get a FREE API key from https://blockfrost.io\n' +
          'Add to frontend/.env:\n' +
          'VITE_BLOCKFROST_PROJECT_ID=your_key_here'
        );
      }
      
      // Get latest protocol parameters
      const protocolResponse = await fetch(
        'https://cardano-preprod.blockfrost.io/api/v0/epochs/latest/parameters',
        {
          headers: { 'project_id': blockfrostApiKey },
        }
      );
      
      if (!protocolResponse.ok) {
        throw new Error('Failed to fetch protocol parameters');
      }
      
      const protocolParams = await protocolResponse.json();
      
      // Get UTXOs from wallet
      const utxosHex = await this.wallet.getUtxos();
      if (!utxosHex || utxosHex.length === 0) {
        throw new Error('No UTXOs available in wallet');
      }
      
      // Get change address
      const changeAddressHex = await this.wallet.getChangeAddress();
      const changeAddressBytes = new Uint8Array(changeAddressHex.match(/.{1,2}/g)!.map(byte => parseInt(byte, 16)));
      const changeAddress = CSL.Address.from_bytes(changeAddressBytes);
      
      // Build transaction
      const txBuilder = CSL.TransactionBuilder.new(
        CSL.TransactionBuilderConfigBuilder.new()
          .fee_algo(
            CSL.LinearFee.new(
              CSL.BigNum.from_str(protocolParams.min_fee_a.toString()),
              CSL.BigNum.from_str(protocolParams.min_fee_b.toString())
            )
          )
          .pool_deposit(CSL.BigNum.from_str(protocolParams.pool_deposit))
          .key_deposit(CSL.BigNum.from_str(protocolParams.key_deposit))
          .max_value_size(protocolParams.max_val_size)
          .max_tx_size(protocolParams.max_tx_size)
          .coins_per_utxo_byte(CSL.BigNum.from_str(protocolParams.coins_per_utxo_size))
          .build()
      );
      
      // Add output
      const recipientAddress = CSL.Address.from_bech32(params.recipient);
      txBuilder.add_output(
        CSL.TransactionOutput.new(
          recipientAddress,
          CSL.Value.new(CSL.BigNum.from_str(params.amount.toString()))
        )
      );
      
      // Add metadata if provided
      if (params.metadata) {
        const metadata = CSL.GeneralTransactionMetadata.new();
        const metadataJson = CSL.encode_json_str_to_metadatum(
          JSON.stringify(params.metadata),
          CSL.MetadataJsonSchema.BasicConversions
        );
        metadata.insert(CSL.BigNum.from_str('674'), metadataJson);
        
        const auxData = CSL.AuxiliaryData.new();
        auxData.set_metadata(metadata);
        txBuilder.set_auxiliary_data(auxData);
      }
      
      // Add inputs from UTXOs
      const txUnspentOutputs = CSL.TransactionUnspentOutputs.new();
      for (const utxoHex of utxosHex) {
        const utxoBytes = new Uint8Array(utxoHex.match(/.{1,2}/g)!.map(byte => parseInt(byte, 16)));
        const utxo = CSL.TransactionUnspentOutput.from_bytes(utxoBytes);
        txUnspentOutputs.add(utxo);
      }
      
      txBuilder.add_inputs_from(txUnspentOutputs, CSL.CoinSelectionStrategyCIP2.LargestFirstMultiAsset);
      
      // Add change
      txBuilder.add_change_if_needed(changeAddress);
      
      // Build transaction body
      const txBody = txBuilder.build();
      
      // Create witness set placeholder
      const witnessSet = CSL.TransactionWitnessSet.new();
      
      // Create transaction
      const transaction = CSL.Transaction.new(
        txBody,
        witnessSet,
        txBuilder.get_auxiliary_data()
      );
      
      // Convert to hex for wallet signing
      const txBytes = transaction.to_bytes();
      const txHex = Array.from(txBytes).map(b => b.toString(16).padStart(2, '0')).join('');
      
      // Sign with wallet
      console.log('Requesting wallet signature...');
      const witnessSetHex = await this.wallet.signTx(txHex, true);
      
      // Combine transaction with witnesses
      const witnessSetBytes = new Uint8Array(witnessSetHex.match(/.{1,2}/g)!.map(byte => parseInt(byte, 16)));
      const witnessSetSigned = CSL.TransactionWitnessSet.from_bytes(witnessSetBytes);
      const signedTx = CSL.Transaction.new(
        transaction.body(),
        witnessSetSigned,
        transaction.auxiliary_data()
      );
      
      console.log('✅ Transaction signed successfully');
      
      // Return signed transaction as hex
      const signedTxBytes = signedTx.to_bytes();
      return Array.from(signedTxBytes).map(b => b.toString(16).padStart(2, '0')).join('');
      
    } catch (error: any) {
      console.error('❌ Failed to build transaction:', error);
      throw error;
    }
  }

  async buildMilestonePayment(params: {
    projectId: string;
    milestoneId: number;
    recipient: string;
    amount: number;
    projectTitle: string;
    milestoneDescription: string;
  }): Promise<string> {
    console.log('💰 Building milestone payment transaction...');
    
    // Truncate strings to fit Cardano's 64-character metadata limit
    const truncate = (str: string, maxLen: number = 60) => 
      str.length > maxLen ? str.substring(0, maxLen) + '...' : str;
    
    const metadata = {
      type: 'payment',
      pid: params.projectId.substring(0, 20),
      mid: params.milestoneId,
      title: truncate(params.projectTitle, 60),
      desc: truncate(params.milestoneDescription, 60),
      amt: params.amount,
      ts: new Date().toISOString(),
    };
    
    return this.buildTransaction({
      recipient: params.recipient,
      amount: params.amount,
      metadata,
    });
  }

  async buildMilestoneCompletion(params: {
    projectId: string;
    milestoneId: number;
    clientAddress: string;
    projectTitle: string;
    milestoneDescription: string;
  }): Promise<string> {
    console.log('📝 Building milestone completion transaction...');
    
    // Truncate strings to fit Cardano's 64-character metadata limit
    const truncate = (str: string, maxLen: number = 60) => 
      str.length > maxLen ? str.substring(0, maxLen) + '...' : str;
    
    const metadata = {
      type: 'complete',
      pid: params.projectId.substring(0, 20),
      mid: params.milestoneId,
      title: truncate(params.projectTitle, 60),
      desc: truncate(params.milestoneDescription, 60),
      by: (await this.getAddress()).substring(0, 40),
      ts: new Date().toISOString(),
    };
    
    // Send a small transaction (1 ADA) to the client as proof of completion
    // This creates an on-chain record that the freelancer has finished the work
    return this.buildTransaction({
      recipient: params.clientAddress,
      amount: 1_000_000, // 1 ADA
      metadata,
    });
  }

  async submitTransaction(signedTx: string): Promise<string> {
    if (!this.wallet) throw new Error('Wallet not connected');
    
    console.log('📤 Submitting transaction to Cardano blockchain...');
    
    // Check for valid Blockfrost API key
    const blockfrostApiKey = import.meta.env.VITE_BLOCKFROST_PROJECT_ID;
    
    if (!blockfrostApiKey || blockfrostApiKey === 'preprodDemo123') {
      throw new Error(
        '❌ Valid Blockfrost API key required for real transactions.\n\n' +
        'Setup instructions:\n' +
        '1. Get FREE API key from https://blockfrost.io\n' +
        '2. Create account and new project (Preprod Testnet)\n' +
        '3. Copy your project ID\n' +
        '4. Add to frontend/.env:\n' +
        '   VITE_BLOCKFROST_PROJECT_ID=preprodYourKeyHere\n' +
        '5. Restart server\n\n' +
        'Without a valid API key, real blockchain transactions cannot be made.'
      );
    }
    
    try {
      console.log('📡 Submitting to Cardano network via Blockfrost...');
      
      // Submit transaction to Blockfrost
      const txBytes = new Uint8Array(signedTx.match(/.{1,2}/g)!.map(byte => parseInt(byte, 16)));
      const response = await fetch(
        'https://cardano-preprod.blockfrost.io/api/v0/tx/submit',
        {
          method: 'POST',
          headers: {
            'project_id': blockfrostApiKey,
            'Content-Type': 'application/cbor',
          },
          body: txBytes,
        }
      );
      
      if (!response.ok) {
        const error = await response.text();
        throw new Error(`Transaction submission failed: ${error}`);
      }
      
      // Blockfrost returns the tx hash as a JSON string with quotes
      const txHashRaw = await response.text();
      const txHash = txHashRaw.replace(/"/g, ''); // Remove quotes
      
      console.log('✅ Transaction submitted successfully to Cardano blockchain!');
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
