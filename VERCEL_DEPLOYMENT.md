# Vercel Deployment Guide for TrustlessTask

## Prerequisites

1. **Vercel Account** - Sign up at https://vercel.com
2. **GitHub Repository** - Already set up at https://github.com/Campverse/TrustlessTask
3. **Blockfrost API Key** - Get from https://blockfrost.io

## Deployment Steps

### Option 1: Deploy via Vercel Dashboard (Recommended)

1. **Go to Vercel Dashboard**
   - Visit https://vercel.com/dashboard
   - Click "Add New Project"

2. **Import GitHub Repository**
   - Click "Import Git Repository"
   - Select "Campverse/TrustlessTask"
   - Click "Import"

3. **Configure Project**
   - **Framework Preset**: Vite
   - **Root Directory**: `frontend`
   - **Build Command**: `npm run build`
   - **Output Directory**: `dist`
   - **Install Command**: `npm install`

4. **Add Environment Variables**
   
   Click "Environment Variables" and add:
   
   ```
   VITE_BLOCKFROST_PROJECT_ID=preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP
   VITE_NETWORK=preprod
   VITE_API_URL=https://your-backend-url.vercel.app/api/v1
   ```
   
   **Important**: You'll need to deploy the backend separately and update `VITE_API_URL`

5. **Deploy**
   - Click "Deploy"
   - Wait for build to complete (~2-3 minutes)
   - Your frontend will be live!

### Option 2: Deploy via Vercel CLI

1. **Install Vercel CLI**
   ```bash
   npm install -g vercel
   ```

2. **Login to Vercel**
   ```bash
   vercel login
   ```

3. **Deploy Frontend**
   ```bash
   cd frontend
   vercel --prod
   ```

4. **Set Environment Variables**
   ```bash
   vercel env add VITE_BLOCKFROST_PROJECT_ID
   # Enter: preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP
   
   vercel env add VITE_NETWORK
   # Enter: preprod
   
   vercel env add VITE_API_URL
   # Enter: https://your-backend-url.vercel.app/api/v1
   ```

5. **Redeploy with Environment Variables**
   ```bash
   vercel --prod
   ```

## Backend Deployment

The backend needs to be deployed separately. Options:

### Option A: Deploy Backend to Vercel

1. **Create New Project for Backend**
   - Go to Vercel Dashboard
   - Click "Add New Project"
   - Import same repository
   - **Root Directory**: `backend-api`
   - **Framework**: Other

2. **Configure Build**
   - **Build Command**: `npm run build` (if you have one) or leave empty
   - **Output Directory**: `.`
   - **Install Command**: `npm install`

3. **Add Environment Variables**
   ```
   NODE_ENV=production
   BLOCKFROST_PROJECT_ID=preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP
   CARDANO_NETWORK=preprod
   ```

4. **Deploy**
   - Click "Deploy"
   - Copy the deployment URL
   - Update frontend's `VITE_API_URL` with this URL

### Option B: Deploy Backend to Railway/Render

For a Node.js backend with database, Railway or Render might be better:

**Railway** (https://railway.app):
- Connect GitHub repository
- Select `backend-api` folder
- Add environment variables
- Deploy

**Render** (https://render.com):
- Create new Web Service
- Connect GitHub repository
- Root directory: `backend-api`
- Build command: `npm install`
- Start command: `npm start`
- Add environment variables

## Post-Deployment Configuration

### 1. Update Frontend Environment Variables

After backend is deployed, update frontend's environment variables:

```bash
vercel env add VITE_API_URL production
# Enter: https://your-backend-url.vercel.app/api/v1
```

Then redeploy:
```bash
cd frontend
vercel --prod
```

### 2. Configure CORS

Update `backend-api/src/server.ts` to allow your Vercel domain:

```typescript
app.use(cors({
  origin: [
    'http://localhost:3000',
    'https://your-app.vercel.app',
    'https://trustlesstask.vercel.app'
  ],
  credentials: true
}));
```

### 3. Update Database

For production, consider using:
- **MongoDB Atlas** (free tier available)
- **PostgreSQL on Railway**
- **Supabase** (PostgreSQL with free tier)

Update database connection in `backend-api/src/db/database.ts`

## Environment Variables Reference

### Frontend (.env)

```env
VITE_API_URL=https://your-backend.vercel.app/api/v1
VITE_BLOCKFROST_PROJECT_ID=preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP
VITE_NETWORK=preprod
```

### Backend (.env)

```env
NODE_ENV=production
PORT=8080
BLOCKFROST_PROJECT_ID=preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP
CARDANO_NETWORK=preprod
DATABASE_URL=your-database-url (if using external DB)
```

## Vercel Configuration Files

The repository includes:

- `vercel.json` - Root configuration
- `frontend/vercel.json` - Frontend-specific config

These files configure:
- Build settings
- Output directories
- Routing rules
- Environment handling

## Testing Deployment

After deployment:

1. **Visit your Vercel URL**
   - Should see TrustlessTask homepage

2. **Test Wallet Connection**
   - Connect Lace/Nami wallet
   - Verify address shows correctly

3. **Test Project Creation**
   - Create a test project
   - Verify it saves to backend

4. **Test Blockchain Transactions**
   - Complete a milestone
   - Approve a milestone
   - Verify transactions on Cardano explorer

## Troubleshooting

### Build Fails

**Problem**: Build fails with module errors

**Solution**:
- Check `package.json` has all dependencies
- Verify Node version (use Node 18+)
- Check build logs in Vercel dashboard

### Environment Variables Not Loading

**Problem**: API key not found

**Solution**:
- Verify environment variables are set in Vercel dashboard
- Redeploy after adding variables
- Check variable names match exactly (case-sensitive)

### CORS Errors

**Problem**: Frontend can't connect to backend

**Solution**:
- Update CORS configuration in backend
- Add Vercel domain to allowed origins
- Redeploy backend

### WASM Loading Issues

**Problem**: Lucid WASM module fails to load

**Solution**:
- Verify `vite.config.ts` has WASM plugins
- Check `vercel.json` build configuration
- May need to use different transaction library

## Custom Domain (Optional)

1. **Go to Project Settings**
   - Click on your project
   - Go to "Settings" → "Domains"

2. **Add Custom Domain**
   - Enter your domain (e.g., trustlesstask.com)
   - Follow DNS configuration instructions

3. **Update Environment Variables**
   - Update CORS settings with new domain
   - Redeploy

## Monitoring

Vercel provides:
- **Analytics** - Page views, performance
- **Logs** - Real-time function logs
- **Speed Insights** - Performance metrics

Access from project dashboard.

## Costs

**Vercel Free Tier includes:**
- ✅ Unlimited deployments
- ✅ 100GB bandwidth/month
- ✅ Automatic HTTPS
- ✅ Preview deployments
- ✅ Analytics

**Upgrade needed for:**
- More bandwidth
- Team collaboration
- Advanced analytics
- Custom deployment regions

## Production Checklist

Before going live:

- [ ] Backend deployed and accessible
- [ ] Frontend deployed and accessible
- [ ] Environment variables configured
- [ ] CORS properly configured
- [ ] Database set up (if using external)
- [ ] Blockfrost API key added
- [ ] Test wallet connection
- [ ] Test project creation
- [ ] Test blockchain transactions
- [ ] Custom domain configured (optional)
- [ ] Analytics enabled
- [ ] Error monitoring set up

## Support

- **Vercel Docs**: https://vercel.com/docs
- **Vercel Discord**: https://vercel.com/discord
- **GitHub Issues**: https://github.com/Campverse/TrustlessTask/issues

## Quick Deploy Button

Add this to your README.md:

```markdown
[![Deploy with Vercel](https://vercel.com/button)](https://vercel.com/new/clone?repository-url=https://github.com/Campverse/TrustlessTask&project-name=trustlesstask&repository-name=trustlesstask)
```

This creates a one-click deploy button!

## Next Steps

1. Deploy frontend to Vercel
2. Deploy backend (Vercel/Railway/Render)
3. Configure environment variables
4. Test thoroughly
5. Share your live URL!

Your TrustlessTask will be live at: `https://your-project.vercel.app`
