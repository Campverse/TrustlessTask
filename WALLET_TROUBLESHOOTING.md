# Wallet Detection Troubleshooting Guide

## Issue: `window.cardano` does not exist

This means no Cardano wallet extensions are being detected by the browser.

## Possible Causes & Solutions

### 1. Extension Not Installed
**Check:**
- Open your browser's extension manager
- Look for Lace, Nami, Eternl, or Flint wallet

**Solution:**
- Install Lace: https://www.lace.io/
- Or install Nami: https://namiwallet.io/

### 2. Extension Disabled
**Check:**
- Go to browser extensions (chrome://extensions/ or edge://extensions/)
- Look for your Cardano wallet
- Check if it's enabled

**Solution:**
- Toggle the extension ON
- Refresh the TrustlessTask page

### 3. Extension Not Loaded Yet
**Check:**
- The extension icon should appear in your browser toolbar
- Click the extension icon to open it

**Solution:**
- Wait a few seconds after page load
- Refresh the page (F5)
- Try clicking the extension icon first, then refresh

### 4. Browser Blocking Extensions
**Check:**
- Some browsers block extensions in incognito/private mode
- Some security software blocks extensions

**Solution:**
- Use normal browsing mode (not incognito)
- Check your antivirus/security software settings
- Try a different browser (Chrome, Edge, Brave)

### 5. Extension Permissions
**Check:**
- The extension needs permission to run on localhost

**Solution:**
- Right-click the extension icon
- Go to "Manage Extension"
- Under "Site Access", select "On all sites" or add localhost

### 6. Browser Compatibility
**Supported Browsers:**
- ✅ Chrome
- ✅ Edge
- ✅ Brave
- ❌ Firefox (limited support)
- ❌ Safari (not supported)

**Solution:**
- Use Chrome, Edge, or Brave browser

## Manual Test

Open browser console (F12) and type:

```javascript
console.log('window.cardano:', window.cardano);
console.log('Available wallets:', window.cardano ? Object.keys(window.cardano) : 'none');
```

**Expected Output (if wallet installed):**
```
window.cardano: {lace: {...}, ...}
Available wallets: ['lace']
```

**Actual Output (current issue):**
```
window.cardano: undefined
Available wallets: none
```

## Step-by-Step Fix

1. **Install Lace Wallet**
   - Go to https://www.lace.io/
   - Click "Download"
   - Install the browser extension
   - Complete wallet setup

2. **Verify Installation**
   - Look for Lace icon in browser toolbar
   - Click it to open the wallet
   - Make sure it's unlocked

3. **Check Permissions**
   - Right-click Lace icon → "Manage Extension"
   - Ensure "Site Access" is enabled
   - Add localhost:3000 and localhost:3001 to allowed sites

4. **Refresh TrustlessTask**
   - Press F5 to refresh
   - Wait 5 seconds
   - Check if "Wallets Detected" appears

5. **Test Connection**
   - Click "Connect Wallet"
   - Select "Lace"
   - Approve the connection in Lace popup

## Still Not Working?

### Try This:
1. Close all browser tabs
2. Restart your browser
3. Open Lace extension first
4. Then open TrustlessTask
5. Try connecting again

### Check Console:
Open browser console (F12) and look for:
- Red errors about extensions
- Security/CORS errors
- Extension loading errors

### Alternative: Use Different Browser
If nothing works, try:
1. Install Chrome or Brave
2. Install Lace extension
3. Open TrustlessTask in that browser

## Contact Support

If you've tried everything and it still doesn't work:
1. Take a screenshot of browser console (F12)
2. Note your browser name and version
3. Note your operating system
4. Share this information for help

## Quick Checklist

- [ ] Lace extension installed
- [ ] Extension is enabled
- [ ] Extension icon visible in toolbar
- [ ] Wallet is unlocked
- [ ] Using supported browser (Chrome/Edge/Brave)
- [ ] Not in incognito mode
- [ ] Extension has site permissions
- [ ] Page refreshed after installation
- [ ] Waited 5+ seconds for detection
- [ ] Checked browser console for errors
