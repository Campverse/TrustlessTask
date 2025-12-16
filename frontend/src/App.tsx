import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useState } from 'react';
import { useWallet } from './hooks/useWallet';
import { ProjectsPage } from './pages/ProjectsPage';
import { CreateProjectPage } from './pages/CreateProjectPage';
import { ProjectDetailPage } from './pages/ProjectDetailPage';
import { ProfilePage } from './pages/ProfilePage';
import { WalletDetector } from './components/WalletDetector';

const queryClient = new QueryClient();

function App() {
  const { wallet, connectWallet, disconnectWallet } = useWallet();
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const [walletMenuOpen, setWalletMenuOpen] = useState(false);

  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <div className="min-h-screen bg-gray-50">
          {/* Navigation */}
          <nav className="bg-white shadow-sm sticky top-0 z-40">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
              <div className="flex justify-between h-16 items-center">
                {/* Logo */}
                <Link to="/" className="text-xl sm:text-2xl font-bold text-blue-600 flex-shrink-0">
                  TrustlessTask
                </Link>

                {/* Desktop Navigation */}
                <div className="hidden md:flex items-center space-x-6">
                  <Link to="/projects" className="text-gray-700 hover:text-blue-600 transition">
                    Projects
                  </Link>
                  <Link to="/create" className="text-gray-700 hover:text-blue-600 transition">
                    Create
                  </Link>
                  {wallet.connected && (
                    <Link to="/profile" className="text-gray-700 hover:text-blue-600 transition">
                      Profile
                    </Link>
                  )}
                </div>

                {/* Wallet Section - Desktop */}
                <div className="hidden md:block">
                  {wallet.connected ? (
                    <div className="flex items-center space-x-3">
                      {wallet.walletName && (
                        <span className="text-xs px-2 py-1 bg-blue-100 text-blue-800 rounded font-semibold uppercase">
                          {wallet.walletName}
                        </span>
                      )}
                      <span className="text-sm text-gray-600 font-medium">
                        {(wallet.balance / 1_000_000).toFixed(2)} ₳
                      </span>
                      {wallet.address && (
                        <span className="text-sm text-gray-600 hidden lg:inline">
                          {wallet.address.slice(0, 8)}...{wallet.address.slice(-6)}
                        </span>
                      )}
                      <button
                        onClick={disconnectWallet}
                        className="px-4 py-2 bg-gray-200 rounded-lg hover:bg-gray-300 transition text-sm"
                      >
                        Disconnect
                      </button>
                    </div>
                  ) : (
                    <div className="relative">
                      <button
                        onClick={() => setWalletMenuOpen(!walletMenuOpen)}
                        className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 transition"
                      >
                        Connect Wallet
                      </button>
                      {walletMenuOpen && (
                        <>
                          <div className="fixed inset-0 z-10" onClick={() => setWalletMenuOpen(false)} />
                          <div className="absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-xl py-2 z-20 border">
                            <button
                              onClick={() => { connectWallet('nami'); setWalletMenuOpen(false); }}
                              className="block w-full text-left px-4 py-3 hover:bg-gray-50 transition"
                            >
                              <div className="font-medium">Nami</div>
                              <div className="text-xs text-gray-500">Via Lace if installed</div>
                            </button>
                            <button
                              onClick={() => { connectWallet('lace'); setWalletMenuOpen(false); }}
                              className="block w-full text-left px-4 py-3 hover:bg-gray-50 transition"
                            >
                              <div className="font-medium">Lace</div>
                              <div className="text-xs text-gray-500">Includes Nami support</div>
                            </button>
                            <button
                              onClick={() => { connectWallet('eternl'); setWalletMenuOpen(false); }}
                              className="block w-full text-left px-4 py-3 hover:bg-gray-50 transition"
                            >
                              Eternl
                            </button>
                            <button
                              onClick={() => { connectWallet('flint'); setWalletMenuOpen(false); }}
                              className="block w-full text-left px-4 py-3 hover:bg-gray-50 transition"
                            >
                              Flint
                            </button>
                          </div>
                        </>
                      )}
                    </div>
                  )}
                </div>

                {/* Mobile Menu Button */}
                <button
                  onClick={() => setMobileMenuOpen(!mobileMenuOpen)}
                  className="md:hidden p-2 rounded-lg hover:bg-gray-100 transition"
                >
                  <svg className="w-6 h-6" fill="none" stroke="currentColor" viewBox="0 0 24 24">
                    {mobileMenuOpen ? (
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M6 18L18 6M6 6l12 12" />
                    ) : (
                      <path strokeLinecap="round" strokeLinejoin="round" strokeWidth={2} d="M4 6h16M4 12h16M4 18h16" />
                    )}
                  </svg>
                </button>
              </div>

              {/* Mobile Menu */}
              {mobileMenuOpen && (
                <div className="md:hidden border-t py-4 space-y-3">
                  <Link
                    to="/projects"
                    onClick={() => setMobileMenuOpen(false)}
                    className="block px-4 py-2 text-gray-700 hover:bg-gray-50 rounded transition"
                  >
                    Projects
                  </Link>
                  <Link
                    to="/create"
                    onClick={() => setMobileMenuOpen(false)}
                    className="block px-4 py-2 text-gray-700 hover:bg-gray-50 rounded transition"
                  >
                    Create Project
                  </Link>
                  {wallet.connected && (
                    <Link
                      to="/profile"
                      onClick={() => setMobileMenuOpen(false)}
                      className="block px-4 py-2 text-gray-700 hover:bg-gray-50 rounded transition"
                    >
                      Profile
                    </Link>
                  )}
                  
                  <div className="border-t pt-3 px-4">
                    {wallet.connected ? (
                      <div className="space-y-3">
                        <div className="flex items-center justify-between">
                          {wallet.walletName && (
                            <span className="text-xs px-2 py-1 bg-blue-100 text-blue-800 rounded font-semibold uppercase">
                              {wallet.walletName}
                            </span>
                          )}
                          <span className="text-sm text-gray-600 font-medium">
                            {(wallet.balance / 1_000_000).toFixed(2)} ₳
                          </span>
                        </div>
                        {wallet.address && (
                          <div className="text-xs text-gray-500 font-mono break-all">
                            {wallet.address}
                          </div>
                        )}
                        <button
                          onClick={() => { disconnectWallet(); setMobileMenuOpen(false); }}
                          className="w-full px-4 py-2 bg-gray-200 rounded-lg hover:bg-gray-300 transition"
                        >
                          Disconnect Wallet
                        </button>
                      </div>
                    ) : (
                      <div className="space-y-2">
                        <button
                          onClick={() => { connectWallet('nami'); setMobileMenuOpen(false); }}
                          className="w-full text-left px-4 py-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition"
                        >
                          <div className="font-medium">Connect Nami</div>
                          <div className="text-xs text-gray-500">Via Lace if installed</div>
                        </button>
                        <button
                          onClick={() => { connectWallet('lace'); setMobileMenuOpen(false); }}
                          className="w-full text-left px-4 py-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition"
                        >
                          <div className="font-medium">Connect Lace</div>
                          <div className="text-xs text-gray-500">Includes Nami support</div>
                        </button>
                        <button
                          onClick={() => { connectWallet('eternl'); setMobileMenuOpen(false); }}
                          className="w-full text-left px-4 py-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition"
                        >
                          Connect Eternl
                        </button>
                        <button
                          onClick={() => { connectWallet('flint'); setMobileMenuOpen(false); }}
                          className="w-full text-left px-4 py-3 bg-gray-50 rounded-lg hover:bg-gray-100 transition"
                        >
                          Connect Flint
                        </button>
                      </div>
                    )}
                  </div>
                </div>
              )}
            </div>
          </nav>

          {/* Main Content */}
          <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-4 sm:py-8">
            <Routes>
              <Route path="/" element={<ProjectsPage />}/>
              <Route path="/projects" element={<ProjectsPage />} />
              <Route path="/create" element={<CreateProjectPage wallet={wallet} />} />
              <Route path="/project/:id" element={<ProjectDetailPage wallet={wallet} />} />
              <Route path="/profile" element={<ProfilePage wallet={wallet} />} />
            </Routes>
          </main>

          <WalletDetector />
        </div>
      </BrowserRouter>
    </QueryClientProvider>
  );
}

export default App;
