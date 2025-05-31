import React, { useState, useEffect, useCallback, useMemo } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Progress } from '@/components/ui/progress';
import { Switch } from '@/components/ui/switch';
import { Textarea } from '@/components/ui/textarea';
import { 
  Activity,
  AlertCircle,
  AlertTriangle,
  ArrowUpDown,
  BarChart3,
  Binary,
  Bot,
  Calendar,
  Check,
  CheckCircle,
  ChevronDown,
  ChevronRight,
  Clock,
  Cloud,
  CloudOff,
  Code,
  Command,
  Copy,
  Database,
  Download,
  Edit2,
  ExternalLink,
  Eye,
  EyeOff,
  FileJson,
  Filter,
  Folder,
  Globe,
  Heart,
  HelpCircle,
  History,
  Home,
  Info,
  Key,
  Layers,
  Link,
  Loader2,
  Lock,
  LogOut,
  MessageSquare,
  Monitor,
  MoreVertical,
  Network,
  Package,
  Pause,
  Play,
  Plus,
  Power,
  RefreshCw,
  Save,
  Search,
  Server,
  Settings,
  Shield,
  ShieldCheck,
  ShieldOff,
  Sparkles,
  Star,
  Terminal,
  Trash2,
  TrendingUp,
  Upload,
  User,
  Users,
  Wifi,
  WifiOff,
  X,
  Zap
} from 'lucide-react';
import { cn } from '@/lib/utils';
import { SeedServersDialog } from './SeedServersDialog';
import { motion, AnimatePresence } from 'framer-motion';

// Types
interface MCPServer {
  id: string;
  name: string;
  category: string;
  url: string;
  type: 'local' | 'remote';
  auth_type: 'oauth2' | 'api_key' | 'open';
  status: 'connected' | 'disconnected' | 'error' | 'running' | 'stopped' | 'connecting';
  health?: ServerHealth;
  capabilities?: ServerCapability[];
  metadata?: ServerMetadata;
  statistics?: ServerStatistics;
  last_seen?: number;
  created_at?: number;
  updated_at?: number;
}

interface ServerHealth {
  status: 'healthy' | 'degraded' | 'unhealthy' | 'unknown';
  latency: number;
  uptime: number;
  last_check: number;
  error_count: number;
  success_rate: number;
}

interface ServerCapability {
  id: string;
  name: string;
  type: 'tool' | 'resource' | 'prompt';
  description: string;
  schema?: any;
  verified: boolean;
  last_tested?: number;
}

interface ServerMetadata {
  version?: string;
  author?: string;
  license?: string;
  repository?: string;
  documentation?: string;
  tags?: string[];
  dependencies?: string[];
}

interface ServerStatistics {
  total_requests: number;
  successful_requests: number;
  failed_requests: number;
  average_response_time: number;
  total_usage_time: number;
  last_used?: number;
}

interface AuthConfig {
  type: 'oauth2' | 'api_key' | 'open';
  oauth2?: {
    client_id?: string;
    client_secret?: string;
    redirect_uri?: string;
    scope?: string;
    token?: string;
  };
  api_key?: {
    key?: string;
    header_name?: string;
  };
}

// Main Component
export default function AdvancedMCPDashboard() {
  // State
  const [servers, setServers] = useState<MCPServer[]>([]);
  const [selectedServer, setSelectedServer] = useState<MCPServer | null>(null);
  const [loading, setLoading] = useState(false);
  const [activeTab, setActiveTab] = useState('overview');
  const [searchQuery, setSearchQuery] = useState('');
  const [filterCategory, setFilterCategory] = useState<string>('all');
  const [filterStatus, setFilterStatus] = useState<string>('all');
  const [sortBy, setSortBy] = useState<'name' | 'status' | 'health' | 'usage'>('name');
  const [sortOrder, setSortOrder] = useState<'asc' | 'desc'>('asc');
  const [showAdvancedFilters, setShowAdvancedFilters] = useState(false);
  const [autoRefresh, setAutoRefresh] = useState(true);
  const [refreshInterval, setRefreshInterval] = useState(30000);
  const [showSeedServers, setShowSeedServers] = useState(false);
  const [bulkSelection, setBulkSelection] = useState<Set<string>>(new Set());
  const [showAuthManager, setShowAuthManager] = useState(false);
  const [authConfigs, setAuthConfigs] = useState<Map<string, AuthConfig>>(new Map());
  
  // WebSocket for real-time updates
  const [ws, setWs] = useState<WebSocket | null>(null);

  // Load data
  useEffect(() => {
    loadServers();
    connectWebSocket();

    const interval = autoRefresh ? setInterval(loadServers, refreshInterval) : null;
    
    return () => {
      if (interval) clearInterval(interval);
      if (ws) ws.close();
    };
  }, [autoRefresh, refreshInterval]);

  const connectWebSocket = () => {
    const websocket = new WebSocket(`ws://${window.location.host}/ws/mcp`);
    
    websocket.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.type === 'server_update') {
        updateServerInList(data.server);
      } else if (data.type === 'health_update') {
        updateServerHealth(data.server_id, data.health);
      }
    };

    websocket.onerror = (error) => {
      console.error('WebSocket error:', error);
    };

    setWs(websocket);
  };

  const loadServers = async () => {
    setLoading(true);
    try {
      const [localRes, remoteRes] = await Promise.all([
        fetch('/api/mcp/local/servers'),
        fetch('/api/mcp/servers')
      ]);

      const localData = await localRes.json();
      const remoteData = await remoteRes.json();

      const allServers = [
        ...(localData.servers || []).map((s: any) => ({ ...s, type: 'local' })),
        ...(remoteData.servers || []).map((s: any) => ({ ...s, type: 'remote' }))
      ];

      // Load health data for connected servers
      const connectedServers = allServers.filter(s => 
        s.status === 'connected' || s.status === 'running'
      );

      const healthPromises = connectedServers.map(server =>
        checkServerHealth(server.id).catch(() => null)
      );

      const healthResults = await Promise.all(healthPromises);
      
      healthResults.forEach((health, index) => {
        if (health) {
          allServers.find(s => s.id === connectedServers[index].id).health = health;
        }
      });

      setServers(allServers);
    } catch (error) {
      console.error('Failed to load servers:', error);
    } finally {
      setLoading(false);
    }
  };

  const checkServerHealth = async (serverId: string): Promise<ServerHealth | null> => {
    try {
      const startTime = Date.now();
      const response = await fetch(`/api/mcp/servers/${serverId}/health`);
      const latency = Date.now() - startTime;
      
      if (response.ok) {
        const data = await response.json();
        return {
          status: 'healthy',
          latency,
          uptime: data.uptime || 0,
          last_check: Date.now(),
          error_count: data.error_count || 0,
          success_rate: data.success_rate || 100
        };
      }
      
      return {
        status: 'unhealthy',
        latency,
        uptime: 0,
        last_check: Date.now(),
        error_count: 1,
        success_rate: 0
      };
    } catch {
      return null;
    }
  };

  const updateServerInList = (updatedServer: MCPServer) => {
    setServers(prev => prev.map(s => 
      s.id === updatedServer.id ? { ...s, ...updatedServer } : s
    ));
  };

  const updateServerHealth = (serverId: string, health: ServerHealth) => {
    setServers(prev => prev.map(s => 
      s.id === serverId ? { ...s, health } : s
    ));
  };

  // Filtering and sorting
  const filteredServers = useMemo(() => {
    let result = servers;

    // Search filter
    if (searchQuery) {
      const query = searchQuery.toLowerCase();
      result = result.filter(server =>
        server.name.toLowerCase().includes(query) ||
        server.url.toLowerCase().includes(query) ||
        server.category?.toLowerCase().includes(query) ||
        server.metadata?.tags?.some(tag => tag.toLowerCase().includes(query))
      );
    }

    // Category filter
    if (filterCategory !== 'all') {
      result = result.filter(server => server.category === filterCategory);
    }

    // Status filter
    if (filterStatus !== 'all') {
      result = result.filter(server => server.status === filterStatus);
    }

    // Sort
    result.sort((a, b) => {
      let compareValue = 0;
      
      switch (sortBy) {
        case 'name':
          compareValue = a.name.localeCompare(b.name);
          break;
        case 'status':
          compareValue = a.status.localeCompare(b.status);
          break;
        case 'health':
          const aHealth = a.health?.status || 'unknown';
          const bHealth = b.health?.status || 'unknown';
          compareValue = aHealth.localeCompare(bHealth);
          break;
        case 'usage':
          const aUsage = a.statistics?.last_used || 0;
          const bUsage = b.statistics?.last_used || 0;
          compareValue = bUsage - aUsage;
          break;
      }

      return sortOrder === 'asc' ? compareValue : -compareValue;
    });

    return result;
  }, [servers, searchQuery, filterCategory, filterStatus, sortBy, sortOrder]);

  // Categories
  const categories = useMemo(() => {
    const cats = new Set(servers.map(s => s.category).filter(Boolean));
    return ['all', ...Array.from(cats)].sort();
  }, [servers]);

  // Server operations
  const connectServer = async (serverId: string) => {
    const server = servers.find(s => s.id === serverId);
    if (!server) return;

    updateServerInList({ ...server, status: 'connecting' });

    try {
      const endpoint = server.type === 'local' 
        ? `/api/mcp/local/servers/${serverId}/start`
        : `/api/mcp/servers/${serverId}/connect`;

      const response = await fetch(endpoint, { method: 'POST' });
      
      if (response.ok) {
        updateServerInList({ ...server, status: server.type === 'local' ? 'running' : 'connected' });
        await loadServers();
      } else {
        throw new Error('Failed to connect');
      }
    } catch (error) {
      updateServerInList({ ...server, status: 'error' });
      console.error('Failed to connect server:', error);
    }
  };

  const disconnectServer = async (serverId: string) => {
    const server = servers.find(s => s.id === serverId);
    if (!server) return;

    try {
      const endpoint = server.type === 'local'
        ? `/api/mcp/local/servers/${serverId}`
        : `/api/mcp/servers/${serverId}/disconnect`;

      const response = await fetch(endpoint, { 
        method: server.type === 'local' ? 'DELETE' : 'POST' 
      });
      
      if (response.ok) {
        updateServerInList({ ...server, status: server.type === 'local' ? 'stopped' : 'disconnected' });
        await loadServers();
      }
    } catch (error) {
      console.error('Failed to disconnect server:', error);
    }
  };

  const deleteServer = async (serverId: string) => {
    if (!confirm('Are you sure you want to delete this server?')) return;

    try {
      await fetch(`/api/mcp/servers/${serverId}`, { method: 'DELETE' });
      setServers(prev => prev.filter(s => s.id !== serverId));
      if (selectedServer?.id === serverId) {
        setSelectedServer(null);
      }
    } catch (error) {
      console.error('Failed to delete server:', error);
    }
  };

  const testServerCapabilities = async (serverId: string) => {
    try {
      const response = await fetch(`/api/mcp/servers/${serverId}/test`, { 
        method: 'POST' 
      });
      
      if (response.ok) {
        const results = await response.json();
        console.log('Test results:', results);
        await loadServers();
      }
    } catch (error) {
      console.error('Failed to test server:', error);
    }
  };

  // Bulk operations
  const performBulkOperation = async (operation: 'connect' | 'disconnect' | 'delete') => {
    const selectedIds = Array.from(bulkSelection);
    
    for (const serverId of selectedIds) {
      switch (operation) {
        case 'connect':
          await connectServer(serverId);
          break;
        case 'disconnect':
          await disconnectServer(serverId);
          break;
        case 'delete':
          await deleteServer(serverId);
          break;
      }
    }

    setBulkSelection(new Set());
  };

  // Export/Import
  const exportServers = () => {
    const data = {
      servers: servers.map(s => ({
        name: s.name,
        url: s.url,
        category: s.category,
        auth_type: s.auth_type,
        metadata: s.metadata
      })),
      exported_at: new Date().toISOString(),
      version: '1.0'
    };

    const blob = new Blob([JSON.stringify(data, null, 2)], { 
      type: 'application/json' 
    });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = 'mcp-servers.json';
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
  };

  const importServers = async (file: File) => {
    try {
      const text = await file.text();
      const data = JSON.parse(text);
      
      if (data.servers && Array.isArray(data.servers)) {
        for (const server of data.servers) {
          await fetch('/api/mcp/servers', {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify(server)
          });
        }
        
        await loadServers();
      }
    } catch (error) {
      console.error('Failed to import servers:', error);
      alert('Failed to import servers. Please check the file format.');
    }
  };

  // UI Components
  const ServerHealthIndicator = ({ health }: { health?: ServerHealth }) => {
    if (!health) return null;

    const getHealthColor = () => {
      switch (health.status) {
        case 'healthy': return 'text-green-500';
        case 'degraded': return 'text-yellow-500';
        case 'unhealthy': return 'text-red-500';
        default: return 'text-gray-500';
      }
    };

    const getHealthIcon = () => {
      switch (health.status) {
        case 'healthy': return <Heart className="w-4 h-4" />;
        case 'degraded': return <AlertTriangle className="w-4 h-4" />;
        case 'unhealthy': return <AlertCircle className="w-4 h-4" />;
        default: return <HelpCircle className="w-4 h-4" />;
      }
    };

    return (
      <div className="flex items-center gap-2">
        <div className={cn("flex items-center gap-1", getHealthColor())}>
          {getHealthIcon()}
          <span className="text-xs font-medium capitalize">{health.status}</span>
        </div>
        <span className="text-xs text-gray-500">{health.latency}ms</span>
      </div>
    );
  };

  const ServerCard = ({ server }: { server: MCPServer }) => {
    const isSelected = selectedServer?.id === server.id;
    const isInBulkSelection = bulkSelection.has(server.id);
    const [expanded, setExpanded] = useState(false);

    return (
      <motion.div
        layout
        initial={{ opacity: 0, y: 20 }}
        animate={{ opacity: 1, y: 0 }}
        exit={{ opacity: 0, y: -20 }}
        className={cn(
          "border rounded-lg p-4 cursor-pointer transition-all",
          isSelected && "border-blue-500 bg-blue-50 dark:bg-blue-950",
          "hover:shadow-md"
        )}
      >
        <div className="flex justify-between items-start">
          <div className="flex items-start gap-3">
            <input
              type="checkbox"
              checked={isInBulkSelection}
              onChange={(e) => {
                const newSelection = new Set(bulkSelection);
                if (e.target.checked) {
                  newSelection.add(server.id);
                } else {
                  newSelection.delete(server.id);
                }
                setBulkSelection(newSelection);
              }}
              className="mt-1"
              onClick={(e) => e.stopPropagation()}
            />
            
            <div 
              className="flex-1"
              onClick={() => setSelectedServer(server)}
            >
              <div className="flex items-center gap-2">
                <h3 className="font-semibold">{server.name}</h3>
                <Badge variant="outline" className="text-xs">
                  {server.type}
                </Badge>
                <AuthTypeBadge authType={server.auth_type} />
              </div>
              
              <p className="text-sm text-gray-600 mt-1">{server.url}</p>
              
              <div className="flex items-center gap-4 mt-2">
                <StatusBadge status={server.status} />
                <ServerHealthIndicator health={server.health} />
                {server.statistics && (
                  <span className="text-xs text-gray-500">
                    {server.statistics.total_requests} requests
                  </span>
                )}
              </div>

              {server.metadata?.tags && (
                <div className="flex gap-1 mt-2 flex-wrap">
                  {server.metadata.tags.map(tag => (
                    <Badge key={tag} variant="secondary" className="text-xs">
                      {tag}
                    </Badge>
                  ))}
                </div>
              )}
            </div>
          </div>

          <div className="flex items-center gap-2" onClick={(e) => e.stopPropagation()}>
            <Button
              size="sm"
              variant="ghost"
              onClick={() => setExpanded(!expanded)}
            >
              {expanded ? <ChevronDown className="w-4 h-4" /> : <ChevronRight className="w-4 h-4" />}
            </Button>
            
            <ServerActionMenu 
              server={server}
              onConnect={() => connectServer(server.id)}
              onDisconnect={() => disconnectServer(server.id)}
              onDelete={() => deleteServer(server.id)}
              onTest={() => testServerCapabilities(server.id)}
            />
          </div>
        </div>

        <AnimatePresence>
          {expanded && (
            <motion.div
              initial={{ height: 0, opacity: 0 }}
              animate={{ height: 'auto', opacity: 1 }}
              exit={{ height: 0, opacity: 0 }}
              className="mt-4 pt-4 border-t"
            >
              <ServerDetails server={server} />
            </motion.div>
          )}
        </AnimatePresence>
      </motion.div>
    );
  };

  const StatusBadge = ({ status }: { status: string }) => {
    const getStatusColor = () => {
      switch (status) {
        case 'connected':
        case 'running':
          return 'bg-green-100 text-green-700';
        case 'connecting':
          return 'bg-blue-100 text-blue-700';
        case 'disconnected':
        case 'stopped':
          return 'bg-gray-100 text-gray-700';
        case 'error':
          return 'bg-red-100 text-red-700';
        default:
          return 'bg-gray-100 text-gray-700';
      }
    };

    const getStatusIcon = () => {
      switch (status) {
        case 'connected':
        case 'running':
          return <Wifi className="w-3 h-3" />;
        case 'connecting':
          return <Loader2 className="w-3 h-3 animate-spin" />;
        case 'disconnected':
        case 'stopped':
          return <WifiOff className="w-3 h-3" />;
        case 'error':
          return <AlertCircle className="w-3 h-3" />;
        default:
          return null;
      }
    };

    return (
      <Badge className={cn("text-xs flex items-center gap-1", getStatusColor())}>
        {getStatusIcon()}
        {status}
      </Badge>
    );
  };

  const AuthTypeBadge = ({ authType }: { authType: string }) => {
    const getAuthIcon = () => {
      switch (authType) {
        case 'oauth2': return <Shield className="w-3 h-3" />;
        case 'api_key': return <Key className="w-3 h-3" />;
        case 'open': return <Globe className="w-3 h-3" />;
        default: return null;
      }
    };

    const getAuthColor = () => {
      switch (authType) {
        case 'oauth2': return 'bg-green-100 text-green-700';
        case 'api_key': return 'bg-yellow-100 text-yellow-700';
        case 'open': return 'bg-blue-100 text-blue-700';
        default: return 'bg-gray-100 text-gray-700';
      }
    };

    return (
      <Badge className={cn("text-xs flex items-center gap-1", getAuthColor())}>
        {getAuthIcon()}
        {authType}
      </Badge>
    );
  };

  const ServerActionMenu = ({ server, onConnect, onDisconnect, onDelete, onTest }: any) => {
    const [open, setOpen] = useState(false);

    return (
      <div className="relative">
        <Button
          size="sm"
          variant="ghost"
          onClick={() => setOpen(!open)}
        >
          <MoreVertical className="w-4 h-4" />
        </Button>

        {open && (
          <div className="absolute right-0 mt-2 w-48 bg-white dark:bg-gray-800 rounded-md shadow-lg z-10 border">
            {(server.status === 'disconnected' || server.status === 'stopped') ? (
              <button
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-100 dark:hover:bg-gray-700 flex items-center gap-2"
                onClick={() => {
                  onConnect();
                  setOpen(false);
                }}
              >
                <Play className="w-4 h-4" />
                Connect
              </button>
            ) : (
              <button
                className="w-full px-4 py-2 text-left text-sm hover:bg-gray-100 dark:hover:bg-gray-700 flex items-center gap-2"
                onClick={() => {
                  onDisconnect();
                  setOpen(false);
                }}
              >
                <Pause className="w-4 h-4" />
                Disconnect
              </button>
            )}

            <button
              className="w-full px-4 py-2 text-left text-sm hover:bg-gray-100 dark:hover:bg-gray-700 flex items-center gap-2"
              onClick={() => {
                onTest();
                setOpen(false);
              }}
            >
              <Zap className="w-4 h-4" />
              Test Capabilities
            </button>

            <button
              className="w-full px-4 py-2 text-left text-sm hover:bg-gray-100 dark:hover:bg-gray-700 flex items-center gap-2"
              onClick={() => {
                navigator.clipboard.writeText(server.url);
                setOpen(false);
              }}
            >
              <Copy className="w-4 h-4" />
              Copy URL
            </button>

            <div className="border-t my-1" />

            <button
              className="w-full px-4 py-2 text-left text-sm hover:bg-gray-100 dark:hover:bg-gray-700 flex items-center gap-2 text-red-600"
              onClick={() => {
                onDelete();
                setOpen(false);
              }}
            >
              <Trash2 className="w-4 h-4" />
              Delete
            </button>
          </div>
        )}
      </div>
    );
  };

  const ServerDetails = ({ server }: { server: MCPServer }) => {
    return (
      <div className="space-y-4">
        {server.capabilities && server.capabilities.length > 0 && (
          <div>
            <h4 className="text-sm font-semibold mb-2">Capabilities</h4>
            <div className="space-y-1">
              {server.capabilities.map(cap => (
                <div key={cap.id} className="flex items-center justify-between text-sm">
                  <div className="flex items-center gap-2">
                    <Badge variant="outline" className="text-xs">
                      {cap.type}
                    </Badge>
                    <span>{cap.name}</span>
                    {cap.verified && <CheckCircle className="w-3 h-3 text-green-500" />}
                  </div>
                  <span className="text-xs text-gray-500">{cap.description}</span>
                </div>
              ))}
            </div>
          </div>
        )}

        {server.metadata && (
          <div>
            <h4 className="text-sm font-semibold mb-2">Metadata</h4>
            <div className="grid grid-cols-2 gap-2 text-sm">
              {server.metadata.version && (
                <div>
                  <span className="text-gray-500">Version:</span> {server.metadata.version}
                </div>
              )}
              {server.metadata.author && (
                <div>
                  <span className="text-gray-500">Author:</span> {server.metadata.author}
                </div>
              )}
              {server.metadata.license && (
                <div>
                  <span className="text-gray-500">License:</span> {server.metadata.license}
                </div>
              )}
              {server.metadata.repository && (
                <div className="col-span-2">
                  <span className="text-gray-500">Repository:</span>
                  <a 
                    href={server.metadata.repository} 
                    target="_blank" 
                    rel="noopener noreferrer"
                    className="text-blue-500 hover:underline ml-1"
                  >
                    {server.metadata.repository}
                  </a>
                </div>
              )}
            </div>
          </div>
        )}

        {server.statistics && (
          <div>
            <h4 className="text-sm font-semibold mb-2">Statistics</h4>
            <div className="grid grid-cols-3 gap-2 text-sm">
              <div>
                <span className="text-gray-500 block text-xs">Total</span>
                <span className="font-semibold">{server.statistics.total_requests}</span>
              </div>
              <div>
                <span className="text-gray-500 block text-xs">Success</span>
                <span className="font-semibold text-green-600">
                  {server.statistics.successful_requests}
                </span>
              </div>
              <div>
                <span className="text-gray-500 block text-xs">Failed</span>
                <span className="font-semibold text-red-600">
                  {server.statistics.failed_requests}
                </span>
              </div>
            </div>
            
            {server.statistics.average_response_time && (
              <div className="mt-2">
                <span className="text-gray-500 text-xs">Avg Response Time:</span>
                <span className="ml-1 text-sm font-semibold">
                  {server.statistics.average_response_time.toFixed(2)}ms
                </span>
              </div>
            )}
          </div>
        )}
      </div>
    );
  };

  return (
    <div className="h-full flex flex-col">
      {/* Header */}
      <div className="border-b px-6 py-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-2xl font-bold flex items-center gap-2">
              <Layers className="w-6 h-6" />
              Advanced MCP Dashboard
            </h1>
            <p className="text-sm text-gray-600 mt-1">
              Manage, monitor, and optimize your Model Context Protocol servers
            </p>
          </div>

          <div className="flex items-center gap-3">
            <Badge variant="outline" className="px-3 py-1">
              <Activity className="w-3 h-3 mr-1" />
              {servers.filter(s => s.status === 'connected' || s.status === 'running').length} Active
            </Badge>

            <div className="flex items-center gap-2">
              <Label htmlFor="auto-refresh" className="text-sm">Auto Refresh</Label>
              <Switch
                id="auto-refresh"
                checked={autoRefresh}
                onCheckedChange={setAutoRefresh}
              />
            </div>

            <Button
              variant="outline"
              size="sm"
              onClick={loadServers}
              disabled={loading}
            >
              {loading ? (
                <Loader2 className="w-4 h-4 animate-spin" />
              ) : (
                <RefreshCw className="w-4 h-4" />
              )}
            </Button>
          </div>
        </div>

        {/* Quick Stats */}
        <div className="grid grid-cols-5 gap-4 mt-4">
          <Card>
            <CardContent className="p-3">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Total Servers</p>
                  <p className="text-2xl font-bold">{servers.length}</p>
                </div>
                <Server className="w-8 h-8 text-gray-400" />
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardContent className="p-3">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Connected</p>
                  <p className="text-2xl font-bold text-green-600">
                    {servers.filter(s => s.status === 'connected' || s.status === 'running').length}
                  </p>
                </div>
                <Wifi className="w-8 h-8 text-green-400" />
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardContent className="p-3">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Healthy</p>
                  <p className="text-2xl font-bold text-green-600">
                    {servers.filter(s => s.health?.status === 'healthy').length}
                  </p>
                </div>
                <Heart className="w-8 h-8 text-green-400" />
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardContent className="p-3">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Errors</p>
                  <p className="text-2xl font-bold text-red-600">
                    {servers.filter(s => s.status === 'error' || s.health?.status === 'unhealthy').length}
                  </p>
                </div>
                <AlertCircle className="w-8 h-8 text-red-400" />
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardContent className="p-3">
              <div className="flex items-center justify-between">
                <div>
                  <p className="text-sm text-gray-600">Categories</p>
                  <p className="text-2xl font-bold">{categories.length - 1}</p>
                </div>
                <Folder className="w-8 h-8 text-blue-400" />
              </div>
            </CardContent>
          </Card>
        </div>
      </div>

      {/* Main Content */}
      <div className="flex-1 flex overflow-hidden">
        {/* Sidebar */}
        <div className="w-80 border-r flex flex-col">
          {/* Search and Filters */}
          <div className="p-4 border-b space-y-3">
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 w-4 h-4 text-gray-400" />
              <Input
                placeholder="Search servers..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                className="pl-10"
              />
            </div>

            <div className="flex gap-2">
              <select
                value={filterCategory}
                onChange={(e) => setFilterCategory(e.target.value)}
                className="flex-1 px-3 py-2 border rounded-md text-sm"
              >
                {categories.map(cat => (
                  <option key={cat} value={cat}>
                    {cat === 'all' ? 'All Categories' : cat}
                  </option>
                ))}
              </select>

              <select
                value={filterStatus}
                onChange={(e) => setFilterStatus(e.target.value)}
                className="flex-1 px-3 py-2 border rounded-md text-sm"
              >
                <option value="all">All Status</option>
                <option value="connected">Connected</option>
                <option value="disconnected">Disconnected</option>
                <option value="running">Running</option>
                <option value="stopped">Stopped</option>
                <option value="error">Error</option>
              </select>
            </div>

            <div className="flex items-center justify-between">
              <Button
                size="sm"
                variant="ghost"
                onClick={() => setShowAdvancedFilters(!showAdvancedFilters)}
              >
                <Filter className="w-4 h-4 mr-1" />
                {showAdvancedFilters ? 'Hide' : 'Show'} Filters
              </Button>

              <div className="flex items-center gap-1">
                <Button
                  size="sm"
                  variant="ghost"
                  onClick={() => setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc')}
                >
                  <ArrowUpDown className="w-4 h-4" />
                </Button>
                <select
                  value={sortBy}
                  onChange={(e) => setSortBy(e.target.value as any)}
                  className="text-sm border rounded px-2 py-1"
                >
                  <option value="name">Name</option>
                  <option value="status">Status</option>
                  <option value="health">Health</option>
                  <option value="usage">Usage</option>
                </select>
              </div>
            </div>

            {showAdvancedFilters && (
              <div className="space-y-2 pt-2 border-t">
                <Label className="text-xs">Advanced Filters</Label>
                {/* Add more advanced filter options here */}
              </div>
            )}
          </div>

          {/* Action Buttons */}
          <div className="p-4 border-b space-y-2">
            <div className="grid grid-cols-2 gap-2">
              <Button
                size="sm"
                onClick={() => setShowSeedServers(true)}
              >
                <Database className="w-4 h-4 mr-1" />
                Seed Servers
              </Button>

              <Button
                size="sm"
                variant="outline"
                onClick={() => document.getElementById('import-file')?.click()}
              >
                <Upload className="w-4 h-4 mr-1" />
                Import
              </Button>
              <input
                id="import-file"
                type="file"
                accept=".json"
                className="hidden"
                onChange={(e) => {
                  const file = e.target.files?.[0];
                  if (file) importServers(file);
                }}
              />

              <Button
                size="sm"
                variant="outline"
                onClick={exportServers}
                disabled={servers.length === 0}
              >
                <Download className="w-4 h-4 mr-1" />
                Export
              </Button>

              <Button
                size="sm"
                variant="outline"
                onClick={() => setShowAuthManager(true)}
              >
                <Key className="w-4 h-4 mr-1" />
                Auth
              </Button>
            </div>

            {bulkSelection.size > 0 && (
              <div className="flex gap-1">
                <Button
                  size="sm"
                  variant="outline"
                  className="flex-1"
                  onClick={() => performBulkOperation('connect')}
                >
                  Connect ({bulkSelection.size})
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  className="flex-1"
                  onClick={() => performBulkOperation('disconnect')}
                >
                  Disconnect
                </Button>
                <Button
                  size="sm"
                  variant="outline"
                  className="text-red-600"
                  onClick={() => performBulkOperation('delete')}
                >
                  <Trash2 className="w-4 h-4" />
                </Button>
              </div>
            )}
          </div>

          {/* Server List */}
          <div className="flex-1 overflow-y-auto p-4 space-y-2">
            <AnimatePresence>
              {filteredServers.map(server => (
                <ServerCard key={server.id} server={server} />
              ))}
            </AnimatePresence>

            {filteredServers.length === 0 && (
              <div className="text-center py-8 text-gray-500">
                <Server className="w-12 h-12 mx-auto mb-2 opacity-20" />
                <p>No servers found</p>
              </div>
            )}
          </div>
        </div>

        {/* Detail Panel */}
        <div className="flex-1 overflow-y-auto">
          {selectedServer ? (
            <ServerDetailPanel 
              server={selectedServer}
              onClose={() => setSelectedServer(null)}
              onRefresh={loadServers}
            />
          ) : (
            <div className="h-full flex items-center justify-center text-gray-500">
              <div className="text-center">
                <Monitor className="w-16 h-16 mx-auto mb-4 opacity-20" />
                <p className="text-lg">Select a server to view details</p>
                <p className="text-sm mt-2">
                  Or add a new server from our curated list
                </p>
                <Button
                  className="mt-4"
                  onClick={() => setShowSeedServers(true)}
                >
                  <Plus className="w-4 h-4 mr-2" />
                  Browse Seed Servers
                </Button>
              </div>
            </div>
          )}
        </div>
      </div>

      {/* Dialogs */}
      <SeedServersDialog
        open={showSeedServers}
        onOpenChange={setShowSeedServers}
        onServerAdded={loadServers}
      />

      {/* Add more dialogs for auth management, etc. */}
    </div>
  );
}

// Server Detail Panel Component
function ServerDetailPanel({ server, onClose, onRefresh }: {
  server: MCPServer;
  onClose: () => void;
  onRefresh: () => void;
}) {
  const [activeTab, setActiveTab] = useState('overview');
  const [testResults, setTestResults] = useState<any>(null);
  const [testing, setTesting] = useState(false);

  const runCapabilityTest = async () => {
    setTesting(true);
    try {
      const response = await fetch(`/api/mcp/servers/${server.id}/test`, {
        method: 'POST'
      });
      
      if (response.ok) {
        const results = await response.json();
        setTestResults(results);
      }
    } catch (error) {
      console.error('Test failed:', error);
    } finally {
      setTesting(false);
    }
  };

  return (
    <div className="h-full flex flex-col">
      {/* Header */}
      <div className="border-b px-6 py-4">
        <div className="flex justify-between items-center">
          <div>
            <h2 className="text-xl font-semibold flex items-center gap-2">
              {server.name}
              <StatusBadge status={server.status} />
              <AuthTypeBadge authType={server.auth_type} />
            </h2>
            <p className="text-sm text-gray-600 mt-1">{server.url}</p>
          </div>

          <div className="flex items-center gap-2">
            <Button variant="outline" size="sm" onClick={onRefresh}>
              <RefreshCw className="w-4 h-4" />
            </Button>
            <Button variant="ghost" size="sm" onClick={onClose}>
              <X className="w-4 h-4" />
            </Button>
          </div>
        </div>
      </div>

      {/* Tabs */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="flex-1 flex flex-col">
        <TabsList className="w-full justify-start px-6">
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="capabilities">Capabilities</TabsTrigger>
          <TabsTrigger value="monitoring">Monitoring</TabsTrigger>
          <TabsTrigger value="logs">Logs</TabsTrigger>
          <TabsTrigger value="settings">Settings</TabsTrigger>
        </TabsList>

        <div className="flex-1 overflow-y-auto">
          <TabsContent value="overview" className="p-6 space-y-6">
            {/* Health Status */}
            <Card>
              <CardHeader>
                <CardTitle>Health Status</CardTitle>
              </CardHeader>
              <CardContent>
                {server.health ? (
                  <div className="space-y-4">
                    <div className="flex items-center justify-between">
                      <span>Status</span>
                      <ServerHealthIndicator health={server.health} />
                    </div>
                    <div className="flex items-center justify-between">
                      <span>Uptime</span>
                      <span>{formatUptime(server.health.uptime)}</span>
                    </div>
                    <div className="flex items-center justify-between">
                      <span>Success Rate</span>
                      <span className="font-semibold">{server.health.success_rate}%</span>
                    </div>
                    <Progress value={server.health.success_rate} className="mt-2" />
                  </div>
                ) : (
                  <p className="text-gray-500">No health data available</p>
                )}
              </CardContent>
            </Card>

            {/* Statistics */}
            <Card>
              <CardHeader>
                <CardTitle>Usage Statistics</CardTitle>
              </CardHeader>
              <CardContent>
                {server.statistics ? (
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <p className="text-sm text-gray-600">Total Requests</p>
                      <p className="text-2xl font-bold">{server.statistics.total_requests}</p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-600">Avg Response Time</p>
                      <p className="text-2xl font-bold">
                        {server.statistics.average_response_time.toFixed(2)}ms
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-600">Success Rate</p>
                      <p className="text-2xl font-bold text-green-600">
                        {((server.statistics.successful_requests / server.statistics.total_requests) * 100).toFixed(1)}%
                      </p>
                    </div>
                    <div>
                      <p className="text-sm text-gray-600">Total Usage</p>
                      <p className="text-2xl font-bold">
                        {formatDuration(server.statistics.total_usage_time)}
                      </p>
                    </div>
                  </div>
                ) : (
                  <p className="text-gray-500">No statistics available</p>
                )}
              </CardContent>
            </Card>

            {/* Metadata */}
            {server.metadata && (
              <Card>
                <CardHeader>
                  <CardTitle>Server Information</CardTitle>
                </CardHeader>
                <CardContent className="space-y-2">
                  {server.metadata.version && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Version</span>
                      <span>{server.metadata.version}</span>
                    </div>
                  )}
                  {server.metadata.author && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Author</span>
                      <span>{server.metadata.author}</span>
                    </div>
                  )}
                  {server.metadata.license && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">License</span>
                      <span>{server.metadata.license}</span>
                    </div>
                  )}
                  {server.metadata.repository && (
                    <div className="flex justify-between">
                      <span className="text-gray-600">Repository</span>
                      <a 
                        href={server.metadata.repository}
                        target="_blank"
                        rel="noopener noreferrer"
                        className="text-blue-500 hover:underline flex items-center gap-1"
                      >
                        View <ExternalLink className="w-3 h-3" />
                      </a>
                    </div>
                  )}
                </CardContent>
              </Card>
            )}
          </TabsContent>

          <TabsContent value="capabilities" className="p-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex justify-between items-center">
                  <span>Server Capabilities</span>
                  <Button
                    size="sm"
                    onClick={runCapabilityTest}
                    disabled={testing}
                  >
                    {testing ? (
                      <Loader2 className="w-4 h-4 animate-spin mr-2" />
                    ) : (
                      <Zap className="w-4 h-4 mr-2" />
                    )}
                    Test All
                  </Button>
                </CardTitle>
              </CardHeader>
              <CardContent>
                {server.capabilities && server.capabilities.length > 0 ? (
                  <div className="space-y-4">
                    {server.capabilities.map(cap => (
                      <div key={cap.id} className="border rounded-lg p-4">
                        <div className="flex items-center justify-between mb-2">
                          <div className="flex items-center gap-2">
                            <Badge>{cap.type}</Badge>
                            <h4 className="font-semibold">{cap.name}</h4>
                            {cap.verified && (
                              <CheckCircle className="w-4 h-4 text-green-500" />
                            )}
                          </div>
                          <Button size="sm" variant="ghost">
                            <Play className="w-4 h-4" />
                          </Button>
                        </div>
                        <p className="text-sm text-gray-600">{cap.description}</p>
                        {cap.schema && (
                          <details className="mt-2">
                            <summary className="cursor-pointer text-sm text-blue-600">
                              View Schema
                            </summary>
                            <pre className="mt-2 p-2 bg-gray-100 rounded text-xs overflow-x-auto">
                              {JSON.stringify(cap.schema, null, 2)}
                            </pre>
                          </details>
                        )}
                      </div>
                    ))}
                  </div>
                ) : (
                  <p className="text-gray-500">No capabilities detected</p>
                )}

                {testResults && (
                  <div className="mt-4 p-4 bg-gray-100 rounded">
                    <h4 className="font-semibold mb-2">Test Results</h4>
                    <pre className="text-xs overflow-x-auto">
                      {JSON.stringify(testResults, null, 2)}
                    </pre>
                  </div>
                )}
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="monitoring" className="p-6">
            <Card>
              <CardHeader>
                <CardTitle>Real-time Monitoring</CardTitle>
              </CardHeader>
              <CardContent>
                <p className="text-gray-500">Monitoring dashboard coming soon...</p>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="logs" className="p-6">
            <Card>
              <CardHeader>
                <CardTitle>Server Logs</CardTitle>
              </CardHeader>
              <CardContent>
                <p className="text-gray-500">Log viewer coming soon...</p>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="settings" className="p-6">
            <Card>
              <CardHeader>
                <CardTitle>Server Settings</CardTitle>
              </CardHeader>
              <CardContent>
                <p className="text-gray-500">Settings panel coming soon...</p>
              </CardContent>
            </Card>
          </TabsContent>
        </div>
      </Tabs>
    </div>
  );
}

// Helper Components
function StatusBadge({ status }: { status: string }) {
  const getStatusColor = () => {
    switch (status) {
      case 'connected':
      case 'running':
        return 'bg-green-100 text-green-700';
      case 'connecting':
        return 'bg-blue-100 text-blue-700';
      case 'disconnected':
      case 'stopped':
        return 'bg-gray-100 text-gray-700';
      case 'error':
        return 'bg-red-100 text-red-700';
      default:
        return 'bg-gray-100 text-gray-700';
    }
  };

  return (
    <Badge className={cn("text-xs", getStatusColor())}>
      {status}
    </Badge>
  );
}

function AuthTypeBadge({ authType }: { authType: string }) {
  const getAuthColor = () => {
    switch (authType) {
      case 'oauth2': return 'bg-green-100 text-green-700';
      case 'api_key': return 'bg-yellow-100 text-yellow-700';
      case 'open': return 'bg-blue-100 text-blue-700';
      default: return 'bg-gray-100 text-gray-700';
    }
  };

  return (
    <Badge className={cn("text-xs", getAuthColor())}>
      {authType}
    </Badge>
  );
}

function ServerHealthIndicator({ health }: { health?: ServerHealth }) {
  if (!health) return null;

  const getHealthColor = () => {
    switch (health.status) {
      case 'healthy': return 'text-green-500';
      case 'degraded': return 'text-yellow-500';
      case 'unhealthy': return 'text-red-500';
      default: return 'text-gray-500';
    }
  };

  return (
    <div className={cn("flex items-center gap-1 text-sm", getHealthColor())}>
      <Heart className="w-4 h-4" />
      <span className="capitalize">{health.status}</span>
    </div>
  );
}

// Utility functions
function formatUptime(seconds: number): string {
  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  
  if (days > 0) return `${days}d ${hours}h`;
  if (hours > 0) return `${hours}h ${minutes}m`;
  return `${minutes}m`;
}

function formatDuration(ms: number): string {
  const hours = Math.floor(ms / 3600000);
  const minutes = Math.floor((ms % 3600000) / 60000);
  
  if (hours > 0) return `${hours}h ${minutes}m`;
  return `${minutes}m`;
}