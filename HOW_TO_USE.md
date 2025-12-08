# 🚀 How to Use TrustlessTask

## ✅ Current Status: FULLY WORKING

Your TrustlessTask application is **100% functional** and ready to use!

## 🎯 Quick Start (2 minutes)

### 1. Open the Application

```
http://localhost:3001
```

### 2. Connect Wallet

Click **"Connect Wallet"** button (top right)
- Choose any wallet (Nami/Eternl/Flint)
- Demo wallet will connect automatically
- You'll see: Address + 1000 ADA balance

### 3. Create Your First Project

1. Click **"Create Project"**
2. Fill in:
   - **Title**: "Website Development"
   - **Description**: "Build a responsive website"
   - **Freelancer Address**: Any address (or use demo address)
   - **Milestones**: Click "Add Milestone"
     - Description: "Design mockups"
     - Amount: 300000000 (300 ADA)
     - Deadline: Pick a future date

3. Click **"Create Project"**
4. Project created! View it in the list

### 4. Manage Milestones

**As Freelancer:**
- Open project details
- Click **"Mark Complete"** on milestone
- Transaction submitted!

**As Client:**
- Open project details
- Click **"Approve & Release Funds"**
- Funds released to freelancer!

### 5. View Your Profile

- Click **"Profile"** in navigation
- See your stats:
  - Completed projects
  - Total earned
  - Average rating
  - Disputes

## 🎮 Demo Mode vs Real Blockchain

### Current Mode: DEMO ✅

**What Works:**
- ✅ Full UI and features
- ✅ Create projects
- ✅ Manage milestones
- ✅ Track progress
- ✅ User profiles
- ✅ All CRUD operations
- ✅ Data persistence

**What's Simulated:**
- 🔄 Wallet connection (instant)
- 🔄 Transactions (instant)
- 🔄 No real blockchain fees

**Perfect For:**
- Testing the application
- Learning the workflow
- Demos and presentations
- Development

### Real Blockchain Mode 🔗

To enable real Cardano transactions:

1. **Install Wallet**: Get Nami/Eternl/Flint extension
2. **Get Test ADA**: From Cardano faucet
3. **Enable Mode**: Set `USE_REAL_BLOCKCHAIN = true` in `frontend/src/hooks/useWallet.ts`
4. **Restart**: Refresh the application

**What Changes:**
- ✅ Real wallet connection
- ✅ Real blockchain transactions
- ✅ Transaction fees (testnet ADA)
- ✅ On-chain verification
- ✅ View on Cardano explorer

## 📖 Features Guide

### Projects

**Create Project:**
- Set title and description
- Define freelancer
- Add multiple milestones
- Optional arbiter for disputes

**View Projects:**
- See all projects
- Filter by status
- Click to view details
- Track progress

**Manage Project:**
- Complete milestones
- Approve work
- Release payments
- Cancel if needed

### Milestones

**Structure:**
- Description of deliverable
- Payment amount
- Deadline date
- Completion status
- Approval status

**Workflow:**
1. Freelancer completes work
2. Marks milestone complete
3. Client reviews work
4. Client approves milestone
5. Funds released automatically

### User Profiles

**Track:**
- Projects completed
- Total earned/spent
- Average rating
- Dispute history

**Reputation:**
- Builds over time
- Visible to others
- Affects trust score

## 🎨 UI Guide

### Navigation Bar

- **TrustlessTask**: Home/Logo
- **Projects**: View all projects
- **Create Project**: Start new project
- **Profile**: Your stats (when connected)
- **Connect Wallet**: Top right button

### Project Card

Shows:
- Title and description
- Status badge (Created/InProgress/Completed)
- Total amount
- Milestone progress (X/Y completed)
- Progress bar
- Created date

### Project Details

Displays:
- Full project information
- Client and freelancer addresses
- All milestones with status
- Action buttons based on role
- Transaction hash (if available)

### Milestone Card

Shows:
- Milestone number
- Description
- Amount in ADA
- Deadline
- Completion checkbox
- Approval checkbox
- Action buttons

## 💡 Tips & Tricks

### For Clients

1. **Clear Milestones**: Define specific, measurable deliverables
2. **Fair Amounts**: Distribute payment across milestones
3. **Realistic Deadlines**: Give adequate time
4. **Quick Approval**: Review and approve promptly
5. **Use Arbiter**: For large projects, add trusted arbiter

### For Freelancers

1. **Understand Requirements**: Read project description carefully
2. **Meet Deadlines**: Complete work on time
3. **Mark Complete**: Update status when done
4. **Communicate**: Keep client informed
5. **Build Reputation**: Complete projects successfully

### Best Practices

1. **Start Small**: Test with small amounts first
2. **Clear Communication**: Discuss requirements upfront
3. **Document Work**: Keep records of deliverables
4. **Fair Disputes**: Only raise when necessary
5. **Build Trust**: Complete projects successfully

## 🔍 Understanding the Interface

### Status Colors

- **Blue** (Created): Project just created
- **Yellow** (InProgress): Work ongoing
- **Purple** (UnderReview): Awaiting approval
- **Red** (Disputed): Dispute raised
- **Green** (Completed): All done!
- **Gray** (Cancelled): Project cancelled

### Amount Display

- Shown in ADA (Cardano's currency)
- 1 ADA = 1,000,000 lovelace
- Example: 1000000000 lovelace = 1000 ADA

### Address Format

- Starts with `addr_test1` (testnet)
- Or `addr1` (mainnet)
- Long string of characters
- Shortened in UI for readability

## 🎯 Common Workflows

### Complete Project Workflow

```
1. Client creates project with milestones
   ↓
2. Freelancer accepts and starts work
   ↓
3. Freelancer completes milestone
   ↓
4. Freelancer marks complete
   ↓
5. Client reviews work
   ↓
6. Client approves milestone
   ↓
7. Funds released to freelancer
   ↓
8. Repeat for all milestones
   ↓
9. Project completed!
```

### Dispute Workflow

```
1. Issue arises
   ↓
2. Either party raises dispute
   ↓
3. Arbiter reviews evidence
   ↓
4. Arbiter makes decision
   ↓
5. Funds distributed per decision
```

## 📊 Data Persistence

All data is saved in:
```
backend-api/data/db.json
```

You can:
- View all projects
- See user data
- Check disputes
- Backup/restore data

## 🎉 You're Ready!

Start using TrustlessTask now:

1. ✅ Application is running
2. ✅ Connect wallet (demo mode)
3. ✅ Create your first project
4. ✅ Test the complete workflow
5. ✅ Explore all features

**Have fun building the future of freelancing!** 🚀

---

**Need Help?**
- Check WALLET_CONNECTION_GUIDE.md for real wallet setup
- See BLOCKCHAIN_INTEGRATION_GUIDE.md for blockchain details
- Review RUNNING_PROJECT.md for technical info
