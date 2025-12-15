# 📋 Cardano Metadata Structure

## Metadata Constraints

Cardano has strict limits on metadata:
- **Maximum string length**: 64 characters per value
- **Metadata label**: 674 (used for TrustlessTask)
- **Format**: JSON object

## Solution

All strings are truncated to fit within the 64-character limit while preserving essential information.

---

## Completion Transaction Metadata

### Structure
```json
{
  "type": "complete",
  "pid": "project_abc123",
  "mid": 1,
  "title": "Build Website Homepage Design",
  "desc": "Complete responsive homepage with modern UI/UX",
  "by": "addr_test1freelancer...",
  "ts": "2024-12-15T16:30:00.000Z"
}
```

### Field Descriptions
| Field | Full Name | Max Length | Description |
|-------|-----------|------------|-------------|
| `type` | Type | 10 | Transaction type: "complete" |
| `pid` | Project ID | 20 | Truncated project identifier |
| `mid` | Milestone ID | - | Milestone number (integer) |
| `title` | Project Title | 60 | Truncated project title |
| `desc` | Description | 60 | Truncated milestone description |
| `by` | Completed By | 40 | Truncated freelancer address |
| `ts` | Timestamp | 24 | ISO 8601 timestamp |

### Example (Real)
```json
{
  "type": "complete",
  "pid": "project_1734282000",
  "mid": 1,
  "title": "Real Blockchain Test",
  "desc": "Test milestone",
  "by": "addr_test1qz3s3c...",
  "ts": "2024-12-15T16:30:15.123Z"
}
```

---

## Payment Transaction Metadata

### Structure
```json
{
  "type": "payment",
  "pid": "project_abc123",
  "mid": 1,
  "title": "Build Website Homepage Design",
  "desc": "Complete responsive homepage with modern UI/UX",
  "amt": 2000000,
  "ts": "2024-12-15T16:35:00.000Z"
}
```

### Field Descriptions
| Field | Full Name | Max Length | Description |
|-------|-----------|------------|-------------|
| `type` | Type | 10 | Transaction type: "payment" |
| `pid` | Project ID | 20 | Truncated project identifier |
| `mid` | Milestone ID | - | Milestone number (integer) |
| `title` | Project Title | 60 | Truncated project title |
| `desc` | Description | 60 | Truncated milestone description |
| `amt` | Amount | - | Payment amount in lovelace (integer) |
| `ts` | Timestamp | 24 | ISO 8601 timestamp |

### Example (Real)
```json
{
  "type": "payment",
  "pid": "project_1734282000",
  "mid": 1,
  "title": "Real Blockchain Test",
  "desc": "Test milestone",
  "amt": 2000000,
  "ts": "2024-12-15T16:35:42.456Z"
}
```

---

## Truncation Rules

### Project Title
- **Max**: 60 characters
- **Truncation**: Adds "..." if longer
- **Example**: 
  - Original: "Build a Complete E-commerce Platform with Shopping Cart and Payment Integration"
  - Truncated: "Build a Complete E-commerce Platform with Shopping Cart..."

### Milestone Description
- **Max**: 60 characters
- **Truncation**: Adds "..." if longer
- **Example**:
  - Original: "Implement user authentication system with OAuth2, JWT tokens, and password reset functionality"
  - Truncated: "Implement user authentication system with OAuth2, JWT token..."

### Project ID
- **Max**: 20 characters
- **Truncation**: Takes first 20 characters
- **Example**:
  - Original: "project_1734282000123"
  - Truncated: "project_173428200012"

### Wallet Address
- **Max**: 40 characters (for completion metadata)
- **Truncation**: Takes first 40 characters
- **Example**:
  - Original: "addr_test1qz3s3c4nlkg98u0r4y8r9qr5r6r7r8r9r0r1r2r3r4r5r6r7r8r9"
  - Truncated: "addr_test1qz3s3c4nlkg98u0r4y8r9qr5r6"

---

## Viewing Metadata on Blockchain

### Cardano Explorer
Visit: https://preprod.cardanoscan.io/

1. Enter transaction hash
2. Click "Metadata" tab
3. See label 674 with JSON data

### Example View
```
Transaction: a1b2c3d4e5f6...
Label: 674
Data:
{
  "type": "complete",
  "pid": "project_1734282000",
  "mid": 1,
  "title": "Real Blockchain Test",
  "desc": "Test milestone",
  "by": "addr_test1qz3s3c...",
  "ts": "2024-12-15T16:30:15.123Z"
}
```

---

## Why These Limits?

### Cardano Protocol
- Designed for efficiency and scalability
- Prevents blockchain bloat
- Ensures fast transaction processing
- Keeps fees low

### Our Solution
- Truncate long strings intelligently
- Preserve essential information
- Use abbreviated field names
- Store full details in database

---

## Full Data Storage

### On Blockchain (Metadata)
- ✅ Transaction type
- ✅ Project/milestone IDs
- ✅ Truncated titles/descriptions
- ✅ Amounts and timestamps
- ✅ Immutable proof

### In Database
- ✅ Full project title
- ✅ Full milestone description
- ✅ Complete wallet addresses
- ✅ Additional project details
- ✅ Transaction hashes (linking to blockchain)

### Best of Both Worlds
- **Blockchain**: Immutable proof with essential info
- **Database**: Full details for user interface
- **Link**: Transaction hash connects both

---

## Code Implementation

### Truncation Function
```typescript
const truncate = (str: string, maxLen: number = 60) => 
  str.length > maxLen ? str.substring(0, maxLen) + '...' : str;
```

### Usage
```typescript
const metadata = {
  type: 'complete',
  pid: params.projectId.substring(0, 20),
  mid: params.milestoneId,
  title: truncate(params.projectTitle, 60),
  desc: truncate(params.milestoneDescription, 60),
  by: (await this.getAddress()).substring(0, 40),
  ts: new Date().toISOString(),
};
```

---

## Verification

### Check Metadata Length
All values must be ≤ 64 characters:
- ✅ "complete" (8 chars)
- ✅ "project_1734282000" (20 chars)
- ✅ 1 (integer, no limit)
- ✅ "Real Blockchain Test" (20 chars)
- ✅ "Test milestone" (14 chars)
- ✅ "addr_test1qz3s3c..." (40 chars)
- ✅ "2024-12-15T16:30:15.123Z" (24 chars)

### Error Prevention
If any string exceeds 64 characters, Lucid will throw:
```
Error: Max metadata string too long: 108, max = 64
```

Our truncation prevents this error.

---

## Future Enhancements

### IPFS Integration
For very long descriptions:
1. Store full content on IPFS
2. Include IPFS hash in metadata
3. Retrieve full content when needed

### Compression
For complex data:
1. Compress JSON data
2. Store compressed string
3. Decompress when reading

### Multiple Labels
For extensive metadata:
1. Split across multiple labels (674, 675, etc.)
2. Link related metadata
3. Reconstruct full data

---

**Status**: ✅ Metadata structure optimized for Cardano limits
**Max String Length**: 64 characters (enforced)
**Truncation**: Automatic with "..." indicator
**Last Updated**: December 15, 2024
