// Global type declarations for Cardano wallet integration

interface CardanoWalletApi {
  getNetworkId(): Promise<number>;
  getUtxos(): Promise<string[] | undefined>;
  getBalance(): Promise<string>;
  getUsedAddresses(): Promise<string[]>;
  getUnusedAddresses(): Promise<string[]>;
  getChangeAddress(): Promise<string>;
  getRewardAddresses(): Promise<string[]>;
  signTx(tx: string, partialSign: boolean): Promise<string>;
  signData(address: string, payload: string): Promise<{ signature: string; key: string }>;
  submitTx(tx: string): Promise<string>;
}

interface CardanoWalletProvider {
  enable(): Promise<CardanoWalletApi>;
  isEnabled(): Promise<boolean>;
}

interface CardanoProviders {
  nami?: CardanoWalletProvider;
  eternl?: CardanoWalletProvider;
  flint?: CardanoWalletProvider;
  lace?: CardanoWalletProvider;
  [key: string]: CardanoWalletProvider | undefined;
}

interface Window {
  cardano?: CardanoProviders;
}
