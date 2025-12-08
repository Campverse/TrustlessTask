#!/bin/bash

# TrustlessTask Deployment Script
# Deploys Plutus smart contracts to Cardano testnet

set -e

echo "🚀 TrustlessTask Deployment Script"
echo "===================================="

# Configuration
NETWORK="testnet-magic 1"
CARDANO_NODE_SOCKET_PATH="${CARDANO_NODE_SOCKET_PATH:-/path/to/node.socket}"

# Build contracts
echo "📦 Building Plutus contracts..."
cd contracts
cabal build
cd ..

# Generate script addresses
echo "🔑 Generating script addresses..."
cabal run trustless-task-contracts -- write-scripts \
  --out-dir ./scripts/plutus-scripts

# Create escrow script address
ESCROW_SCRIPT="./scripts/plutus-scripts/escrow.plutus"
cardano-cli address build \
  --payment-script-file $ESCROW_SCRIPT \
  --$NETWORK \
  --out-file ./scripts/addresses/escrow.addr

echo "✅ Escrow address: $(cat ./scripts/addresses/escrow.addr)"

# Create dispute script address
DISPUTE_SCRIPT="./scripts/plutus-scripts/dispute.plutus"
cardano-cli address build \
  --payment-script-file $DISPUTE_SCRIPT \
  --$NETWORK \
  --out-file ./scripts/addresses/dispute.addr

echo "✅ Dispute address: $(cat ./scripts/addresses/dispute.addr)"

# Create reputation script address
REPUTATION_SCRIPT="./scripts/plutus-scripts/reputation.plutus"
cardano-cli address build \
  --payment-script-file $REPUTATION_SCRIPT \
  --$NETWORK \
  --out-file ./scripts/addresses/reputation.addr

echo "✅ Reputation address: $(cat ./scripts/addresses/reputation.addr)"

echo ""
echo "✨ Deployment complete!"
echo "Update your .env files with these addresses"
