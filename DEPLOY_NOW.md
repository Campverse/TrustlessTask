# 🚀 Deploy TrustlessTask to Vercel NOW

## Quick Deploy (5 Minutes)

### Step 1: Open Vercel
👉 **Click here**: https://vercel.com/new

### Step 2: Import Repository
1. Click **"Import Git Repository"**
2. If not connected, click **"Connect GitHub"**
3. Search for: **"TrustlessTask"** or **"Campverse/TrustlessTask"**
4. Click **"Import"**

### Step 3: Configure Project

**Framework Preset**: Select **"Vite"**

**Root Directory**: 
- Click **"Edit"**
- Enter: `frontend`
- Click **"Continue"**

**Build Settings** (should auto-detect):
- Build Command: `npm run build`
- Output Directory: `dist`
- Install Command: `npm install`

### Step 4: Add Environment Variables

Click **"Environment Variables"** and add these **3 variables**:

1. **Name**: `VITE_BLOCKFROST_PROJECT_ID`
   **Value**: `preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP`

2. **Name**: `VITE_NETWORK`
   **Value**: `preprod`

3. **Name**: `VITE_API_URL`
   **Value**: `http://localhost:8080/api/v1` (temporary - will update later)

### Step 5: Deploy!

Click **"Deploy"** button

⏳ Wait 2-3 minutes for build to complete...

### Step 6: Get Your URL

Once deployed, you'll see:
```
🎉 Congratulations! Your project is live!
https://trustlesstask-xxx.vercel.app
```

**Copy this URL!**

---

## ✅ Your Frontend is Live!

Visit your URL to see TrustlessTask running!

### What Works Now:
- ✅ Homepage
- ✅ Wallet connection
- ✅ UI/UX
- ⚠️ Backend API (still needs deployment)

---

## Next: Deploy Backend

The frontend is calling `localhost:8080` which won't work in production. You need to deploy the backend.

### Option A: Deploy Backend to Vercel

1. Go back to https://vercel.com/new
2. Import **same repository** again
3. **Root Directory**: `backend-api`
4. **Framework**: Other
5. Add environment variables:
   - `NODE_ENV=production`
   - `BLOCKFROST_PROJECT_ID=preprodJWXJGPovY8gGA1Seasf8gaTnYFZ88rzP`
6. Deploy

### Option B: Deploy Backend to Railway (Recommended)

Railway is better for Node.js backends with databases:

1. Go to https://railway.app
2. Click **"Start a New Project"**
3. Select **"Deploy from GitHub repo"**
4. Choose **"Campverse/TrustlessTask"**
5. **Root directory**: `backend-api`
6. Add environment variables (same as above)
7. Deploy

**Copy the Railway URL** (e.g., `https://trustlesstask-backend.up.railway.app`)

### Update Frontend Environment Variable

1. Go to your Vercel project
2. Click **"Settings"** → **"Environment Variables"**
3. Find `VITE_API_URL`
4. Click **"Edit"**
5. Change to: `https://your-backend-url.railway.app/api/v1`
6. Click **"Save"**
7. Go to **"Deployments"**
8. Click **"..."** on latest deployment
9. Click **"Redeploy"**

---

## 🎉 Done!

Your TrustlessTask is now fully deployed!

### Test It:

1. Visit your Vercel URL
2. Connect wallet (Lace/Nami)
3. Create a project
4. Complete milestone
5. Approve milestone
6. View transaction on https://preprod.cardanoscan.io

---

## Troubleshooting

### Build Fails

**Error**: "Module not found"
**Fix**: Check all dependencies are in `package.json`

### Environment Variables Not Loading

**Error**: "Blockfrost API key required"
**Fix**: 
1. Verify variables are added in Vercel
2. Redeploy after adding variables
3. Hard refresh browser (Ctrl+F5)

### CORS Error

**Error**: "Access blocked by CORS"
**Fix**: Update `backend-api/src/server.ts`:
```typescript
app.use(cors({
  origin: [
    'https://your-frontend.vercel.app',
    'http://localhost:3000'
  ]
}));
```

---

## Custom Domain (Optional)

1. Go to project **Settings** → **Domains**
2. Add your domain (e.g., `trustlesstask.com`)
3. Follow DNS instructions
4. Wait for SSL certificate (~5 minutes)

---

## Monitoring

Vercel Dashboard shows:
- 📊 Analytics
- 📝 Logs
- ⚡ Performance
- 🔄 Deployments

---

## Support

- **Vercel Docs**: https://vercel.com/docs
- **Railway Docs**: https://docs.railway.app
- **GitHub Issues**: https://github.com/Campverse/TrustlessTask/issues

---

## Quick Links

- 🌐 **Deploy Frontend**: https://vercel.com/new/clone?repository-url=https://github.com/Campverse/TrustlessTask&project-name=trustlesstask&root-directory=frontend
- 🚂 **Deploy Backend**: https://railway.app/new
- 📚 **Full Guide**: See `VERCEL_DEPLOYMENT.md`

---

**Ready? Let's deploy!** 🚀

Click here to start: https://vercel.com/new
