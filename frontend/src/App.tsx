import React from 'react';
import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useWallet } from './hooks/useWallet';
import { ProjectsPage } from './pages/ProjectsPage';
import { CreateProjectPage } from './pages/CreateProjectPage';
import { ProjectDetailPage } from './pages/ProjectDetailPage';
import { ProfilePage } from './pages/ProfilePage';

const queryClient = new QueryClient();

function App() {
  const { wallet, connectWallet, disconnectWallet } = useWallet();

  return (
    <QueryClientProvider client={queryClient}>
      <BrowserRouter>
        <div className="min-h-screen bg-gray-50">
          <nav className="bg-white shadow-sm">
            <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8">
              <div className="flex justify-between h-16 items-center">
                <div className="flex items-center space-x-8">
                  <Link to="/" className="text-2xl font-bold text-blue-600">
                    TrustlessTask
                  </Link>
                  <Link to="/projects" className="text-gray-700 hover:text-blue-600">
                    Projects
                  </Link>
                  <Link to="/create" className="text-gray-700 hover:text-blue-600">
                    Create Project
                  </Link>
                  {wallet.connected && (
                    <Link to="/profile" className="text-gray-700 hover:text-blue-600">
                      Profile
                    </Link>
                  )}
                </div>

                <div>
                  {wallet.connected ? (
                    <div className="flex items-center space-x-4">
                      <span className="text-sm text-gray-600">
                        {(wallet.balance / 1_000_000).toFixed(2)} ₳
                      </span>
                      <span className="text-sm text-gray-600">
                        {wallet.address?.slice(0, 8)}...{wallet.address?.slice(-8)}
                      </span>
                      <button
                        onClick={disconnectWallet}
                        className="px-4 py-2 bg-gray-200 rounded-lg hover:bg-gray-300"
                      >
                        Disconnect
                      </button>
                    </div>
                  ) : (
                    <button
                      onClick={() => connectWallet('nami')}
                      className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                    >
                      Connect Wallet
                    </button>
                  )}
                </div>
              </div>
            </div>
          </nav>

          <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
            <Routes>
              <Route path="/" element={<ProjectsPage />} />
              <Route path="/projects" element={<ProjectsPage />} />
              <Route path="/create" element={<CreateProjectPage wallet={wallet} />} />
              <Route path="/project/:id" element={<ProjectDetailPage wallet={wallet} />} />
              <Route path="/profile" element={<ProfilePage wallet={wallet} />} />
            </Routes>
          </main>
        </div>
      </BrowserRouter>
    </QueryClientProvider>
  );
}

export default App;
