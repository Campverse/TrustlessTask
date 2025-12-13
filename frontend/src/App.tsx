import { BrowserRouter, Routes, Route, Link } from 'react-router-dom';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { useWallet } from './hooks/useWallet';
import { ProjectsPage } from './pages/ProjectsPage';
import { CreateProjectPage } from './pages/CreateProjectPage';
import { ProjectDetailPage } from './pages/ProjectDetailPage';
import { ProfilePage } from './pages/ProfilePage';
import { WalletDetector } from './components/WalletDetector';

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
                      {wallet.walletName && (
                        <span className="text-xs px-2 py-1 bg-blue-100 text-blue-800 rounded font-semibold uppercase">
                          {wallet.walletName}
                        </span>
                      )}
                      <span className="text-sm text-gray-600">
                        {(wallet.balance / 1_000_000).toFixed(2)} ₳
                      </span>
                      {wallet.address ? (
                        <span className="text-sm text-gray-600">
                          {wallet.address.slice(0, 8)}...{wallet.address.slice(-8)}
                        </span>
                      ) : (
                        <span className="text-sm text-red-600">No address</span>
                      )}
                      <button
                        onClick={disconnectWallet}
                        className="px-4 py-2 bg-gray-200 rounded-lg hover:bg-gray-300"
                      >
                        Disconnect
                      </button>
                    </div>
                  ) : (
                    <div className="relative group">
                      <button
                        className="px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700"
                      >
                        Connect Wallet
                      </button>
                      <div className="hidden group-hover:block absolute right-0 mt-2 w-56 bg-white rounded-lg shadow-lg py-2 z-50">
                        <button
                          onClick={() => connectWallet('nami')}
                          className="block w-full text-left px-4 py-2 hover:bg-gray-100"
                        >
                          <div className="font-medium">Nami</div>
                          <div className="text-xs text-gray-500">Via Lace if installed</div>
                        </button>
                        <button
                          onClick={() => connectWallet('lace')}
                          className="block w-full text-left px-4 py-2 hover:bg-gray-100"
                        >
                          <div className="font-medium">Lace</div>
                          <div className="text-xs text-gray-500">Includes Nami support</div>
                        </button>
                        <button
                          onClick={() => connectWallet('eternl')}
                          className="block w-full text-left px-4 py-2 hover:bg-gray-100"
                        >
                          Eternl
                        </button>
                        <button
                          onClick={() => connectWallet('flint')}
                          className="block w-full text-left px-4 py-2 hover:bg-gray-100"
                        >
                          Flint
                        </button>
                      </div>
                    </div>
                  )}
                </div>
              </div>
            </div>
          </nav>

          <main className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8">
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
