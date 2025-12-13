/// <reference types="vite/client" />

interface ImportMetaEnv {
  readonly VITE_API_URL?: string;
  readonly VITE_BLOCKFROST_PROJECT_ID?: string;
  readonly VITE_NETWORK?: string;
  readonly VITE_USE_REAL_BLOCKCHAIN?: string;
}

interface ImportMeta {
  readonly env: ImportMetaEnv;
}
