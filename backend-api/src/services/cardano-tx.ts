// Cardano Transaction Builder for Backend
// Builds transactions that interact with Plutus smart contracts

import axios from 'axios';

interface BlockfrostConfig {
  projectId: string;
  network: 'preprod' | 'mainnet';
}

export class CardanoTxBuilder {
  private config: BlockfrostConfig;
  private baseUrl: string;

  constructor(config: BlockfrostConfig) {
    this.config = config;
    this.baseUrl = `https://cardano-${config.network}.blockfrost.io/api/v0`;
  }

  async getScriptAddress(scriptHash: string): Promise<string> {
    // Convert script hash to bech32 address
    // This would use cardano-serialization-lib in production
    return `addr_test1w${scriptHash}`;
  }

  async buildLockFundsTx(params: {
    clientAddress: string;
    amount: number;
    scriptAddress: string;
    datum: any;
  }): Promise<{
    txHash: string;
    cbor: string;
  }> {
    // In production, this would:
    // 1. Query client UTxOs
    // 2. Build transaction with script output
    // 3. Attach inline datum
    // 4. Calculate fees
    // 5. Return unsigned transaction

    console.log('Building lock funds transaction:', params);

    // Simulated for now
    return {
      txHash: `tx_lock_${Date.now()}`,
      cbor: 'simulated_cbor_data',
    };
  }

  async buildReleaseFundsTx(params: {
    scriptUtxo: string;
    redeemer: any;
    recipientAddress: string;
    amount: number;
  }): Promise<{
    txHash: string;
    cbor: string;
  }> {
    // In production, this would:
    // 1. Query script UTxO
    // 2. Build transaction spending from script
    // 3. Attach redeemer
    // 4. Include script reference or full script
    // 5. Calculate fees including script execution
    // 6. Return unsigned transaction

    console.log('Building release funds transaction:', params);

    return {
      txHash: `tx_release_${Date.now()}`,
      cbor: 'simulated_cbor_data',
    };
  }

  async submitTransaction(signedTxCbor: string): Promise<string> {
    try {
      const response = await axios.post(
        `${this.baseUrl}/tx/submit`,
        signedTxCbor,
        {
          headers: {
            'project_id': this.config.projectId,
            'Content-Type': 'application/cbor',
          },
        }
      );

      return response.data;
    } catch (error) {
      console.error('Transaction submission failed:', error);
      throw error;
    }
  }

  async queryScriptUtxos(scriptAddress: string): Promise<any[]> {
    try {
      const response = await axios.get(
        `${this.baseUrl}/addresses/${scriptAddress}/utxos`,
        {
          headers: {
            'project_id': this.config.projectId,
          },
        }
      );

      return response.data;
    } catch (error) {
      console.error('Failed to query script UTxOs:', error);
      return [];
    }
  }

  async getTransactionStatus(txHash: string): Promise<{
    confirmed: boolean;
    blockHeight?: number;
    confirmations?: number;
  }> {
    try {
      const response = await axios.get(
        `${this.baseUrl}/txs/${txHash}`,
        {
          headers: {
            'project_id': this.config.projectId,
          },
        }
      );

      const data = response.data;
      return {
        confirmed: true,
        blockHeight: data.block_height,
        confirmations: data.block_height ? await this.getLatestBlock() - data.block_height : 0,
      };
    } catch (error) {
      return { confirmed: false };
    }
  }

  private async getLatestBlock(): Promise<number> {
    try {
      const response = await axios.get(
        `${this.baseUrl}/blocks/latest`,
        {
          headers: {
            'project_id': this.config.projectId,
          },
        }
      );

      return response.data.height;
    } catch (error) {
      return 0;
    }
  }
}

// Singleton instance
let txBuilder: CardanoTxBuilder | null = null;

export function getCardanoTxBuilder(): CardanoTxBuilder {
  if (!txBuilder) {
    const config: BlockfrostConfig = {
      projectId: process.env.BLOCKFROST_PROJECT_ID || 'preprodDemo123',
      network: (process.env.CARDANO_NETWORK?.toLowerCase() || 'preprod') as 'preprod' | 'mainnet',
    };
    txBuilder = new CardanoTxBuilder(config);
  }
  return txBuilder;
}
