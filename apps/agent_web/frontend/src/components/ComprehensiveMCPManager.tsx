import { useState, useEffect } from 'react'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog'
import { 
  Plus, 
  RefreshCw, 
  CheckCircle, 
  XCircle, 
  AlertCircle,
  Server,
  Globe,
  Search,
  Monitor,
  Download,
  Copy,
  ExternalLink,
  Database
} from 'lucide-react'
import { SeedServersDialog } from './SeedServersDialog'
import { MCPOAuthManager } from './MCPOAuthManager'

interface MCPServer {
  id: string
  name?: string
  url?: string
  type: 'local' | 'remote'
  status: 'connected' | 'disconnected' | 'error' | 'running' | 'stopped'
  config?: any
  server_info?: any
  capabilities?: any
  registered_at?: number
  last_seen?: number
  port?: number
}

interface MCPStatus {
  local_servers: number
  remote_clients: number
  auto_connect_enabled: boolean
  discovery_active: boolean
}

export default function ComprehensiveMCPManager() {
  const [localServers, setLocalServers] = useState<MCPServer[]>([])
  const [remoteServers, setRemoteServers] = useState<MCPServer[]>([])
  const [selectedServer, setSelectedServer] = useState<string | null>(null)
  const [mcpStatus, setMcpStatus] = useState<MCPStatus | null>(null)
  const [loading, setLoading] = useState(false)
  const [activeTab, setActiveTab] = useState('overview')
  
  // Dialog states
  const [showAddRemoteDialog, setShowAddRemoteDialog] = useState(false)
  const [showAddLocalDialog, setShowAddLocalDialog] = useState(false)
  const [showDiscoveryDialog, setShowDiscoveryDialog] = useState(false)
  const [showSeedServersDialog, setShowSeedServersDialog] = useState(false)
  
  // Form states
  const [newRemoteUrl, setNewRemoteUrl] = useState('')
  const [newRemoteName, setNewRemoteName] = useState('')
  const [newLocalPort, setNewLocalPort] = useState('8766')
  const [newLocalName, setNewLocalName] = useState('')
  
  // Discovery state
  const [discoveredServers, setDiscoveredServers] = useState<any[]>([])
  const [discovering, setDiscovering] = useState(false)
  
  // Inspector compatibility state
  const [inspectorConfig, setInspectorConfig] = useState<any>({})
  const [_showInspectorDialog, _setShowInspectorDialog] = useState(false)

  useEffect(() => {
    loadAllData()
    const interval = setInterval(loadAllData, 10000) // Refresh every 10 seconds
    return () => clearInterval(interval)
  }, [])

  const loadAllData = async () => {
    await Promise.all([
      loadLocalServers(),
      loadRemoteServers(),
      loadMCPStatus()
    ])
  }

  const loadLocalServers = async () => {
    try {
      const response = await fetch('/api/mcp/local/servers')
      if (response.ok) {
        const data = await response.json()
        setLocalServers(data.servers || [])
      }
    } catch (error) {
      console.error('Failed to load local servers:', error)
    }
  }

  const loadRemoteServers = async () => {
    try {
      const response = await fetch('/api/mcp/servers')
      if (response.ok) {
        const data = await response.json()
        const servers = (data.servers || []).map((server: any) => ({
          ...server,
          type: 'remote'
        }))
        setRemoteServers(servers)
      }
    } catch (error) {
      console.error('Failed to load remote servers:', error)
    }
  }

  const loadMCPStatus = async () => {
    try {
      const response = await fetch('/api/mcp/status')
      if (response.ok) {
        const data = await response.json()
        setMcpStatus(data)
      }
    } catch (error) {
      console.error('Failed to load MCP status:', error)
    }
  }

  const startLocalServer = async () => {
    try {
      setLoading(true)
      const config = {
        server_id: newLocalName || `local_server_${Date.now()}`,
        transport: 'websocket',
        port: parseInt(newLocalPort, 10)
      }

      const response = await fetch('/api/mcp/local/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      })

      if (response.ok) {
        setShowAddLocalDialog(false)
        setNewLocalName('')
        setNewLocalPort('8766')
        await loadLocalServers()
      } else {
        const error = await response.json()
        console.error('Failed to start local server:', error)
      }
    } catch (error) {
      console.error('Failed to start local server:', error)
    } finally {
      setLoading(false)
    }
  }

  const stopLocalServer = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/local/servers/${serverId}`, {
        method: 'DELETE'
      })

      if (response.ok) {
        await loadLocalServers()
        if (selectedServer === serverId) {
          setSelectedServer(null)
        }
      }
    } catch (error) {
      console.error('Failed to stop local server:', error)
    } finally {
      setLoading(false)
    }
  }

  const addRemoteServer = async () => {
    try {
      setLoading(true)
      const config = {
        name: newRemoteName,
        url: newRemoteUrl,
        type: 'remote'
      }

      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      })

      if (response.ok) {
        setShowAddRemoteDialog(false)
        setNewRemoteName('')
        setNewRemoteUrl('')
        await loadRemoteServers()
      }
    } catch (error) {
      console.error('Failed to add remote server:', error)
    } finally {
      setLoading(false)
    }
  }

  const connectRemoteServer = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/connect`, {
        method: 'POST'
      })

      if (response.ok) {
        await loadRemoteServers()
      }
    } catch (error) {
      console.error('Failed to connect to remote server:', error)
    } finally {
      setLoading(false)
    }
  }

  const disconnectRemoteServer = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/disconnect`, {
        method: 'POST'
      })

      if (response.ok) {
        await loadRemoteServers()
        if (selectedServer === serverId) {
          setSelectedServer(null)
        }
      }
    } catch (error) {
      console.error('Failed to disconnect from remote server:', error)
    } finally {
      setLoading(false)
    }
  }

  const discoverServers = async () => {
    try {
      setDiscovering(true)
      const response = await fetch('/api/mcp/discover', {
        method: 'POST'
      })

      if (response.ok) {
        const data = await response.json()
        setDiscoveredServers(data.discovered || [])
        setShowDiscoveryDialog(true)
      }
    } catch (error) {
      console.error('Failed to discover servers:', error)
    } finally {
      setDiscovering(false)
    }
  }

  const connectDiscoveredServer = async (serverInfo: any) => {
    try {
      const config = {
        name: `Discovered ${serverInfo.type}`,
        url: serverInfo.url,
        auto_discovered: true
      }

      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(config)
      })

      if (response.ok) {
        const result = await response.json()
        await connectRemoteServer(result.id)
        setShowDiscoveryDialog(false)
        await loadRemoteServers()
      }
    } catch (error) {
      console.error('Failed to connect discovered server:', error)
    }
  }

  const testConnection = async (url: string) => {
    if (!url) return
    
    try {
      // Create a temporary server entry to test
      const tempConfig = {
        name: 'temp_test',
        url: url,
        type: 'remote'
      }

      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(tempConfig)
      })

      if (response.ok) {
        const result = await response.json()
        
        // Try to test the connection
        const testResponse = await fetch(`/api/mcp/servers/${result.id}/test`, {
          method: 'POST'
        })

        const testResult = await testResponse.json()
        
        // Clean up the temporary server
        await fetch(`/api/mcp/servers/${result.id}`, {
          method: 'DELETE'
        })

        if (testResult.status === 'success') {
          alert('✅ Connection successful! The server is reachable.')
        } else {
          alert(`❌ Connection failed: ${testResult.error || 'Unknown error'}`)
        }
      } else {
        alert('❌ Invalid URL format')
      }
    } catch (error) {
      console.error('Connection test failed:', error)
      alert('❌ Connection test failed: Network error')
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'connected':
      case 'running':
        return <CheckCircle className="h-4 w-4 text-green-500" />
      case 'disconnected':
      case 'stopped':
        return <XCircle className="h-4 w-4 text-gray-500" />
      case 'error':
        return <AlertCircle className="h-4 w-4 text-red-500" />
      default:
        return <AlertCircle className="h-4 w-4 text-yellow-500" />
    }
  }

  const getStatusColor = (status: string) => {
    switch (status) {
      case 'connected':
      case 'running':
        return 'text-green-600 bg-green-50'
      case 'disconnected':
      case 'stopped':
        return 'text-gray-600 bg-gray-50'
      case 'error':
        return 'text-red-600 bg-red-50'
      default:
        return 'text-yellow-600 bg-yellow-50'
    }
  }

  return (
    <div className="space-y-6">
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold">MCP Server Management</h2>
          <p className="text-muted-foreground">
            Comprehensive Model Context Protocol server and client management
          </p>
        </div>
        
        <div className="flex gap-2">
          <Button variant="outline" onClick={loadAllData} disabled={loading}>
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
          
          <Button variant="outline" onClick={discoverServers} disabled={discovering}>
            <Search className="h-4 w-4 mr-2" />
            {discovering ? 'Discovering...' : 'Discover'}
          </Button>
        </div>
      </div>

      {mcpStatus && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Monitor className="h-5 w-5" />
              System Status
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="grid grid-cols-4 gap-4">
              <div className="text-center">
                <div className="text-2xl font-bold text-blue-600">{mcpStatus.local_servers}</div>
                <div className="text-sm text-muted-foreground">Local Servers</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-green-600">{mcpStatus.remote_clients}</div>
                <div className="text-sm text-muted-foreground">Remote Clients</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-purple-600">
                  {mcpStatus.auto_connect_enabled ? 'ON' : 'OFF'}
                </div>
                <div className="text-sm text-muted-foreground">Auto Connect</div>
              </div>
              <div className="text-center">
                <div className="text-2xl font-bold text-orange-600">
                  {mcpStatus.discovery_active ? 'ACTIVE' : 'INACTIVE'}
                </div>
                <div className="text-sm text-muted-foreground">Discovery</div>
              </div>
            </div>
          </CardContent>
        </Card>
      )}

      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList className="grid w-full grid-cols-5">
          <TabsTrigger value="overview">
            <Monitor className="h-4 w-4 mr-2" />
            Overview
          </TabsTrigger>
          <TabsTrigger value="local">
            <Server className="h-4 w-4 mr-2" />
            Local Servers
          </TabsTrigger>
          <TabsTrigger value="remote">
            <Globe className="h-4 w-4 mr-2" />
            Remote Servers
          </TabsTrigger>
          <TabsTrigger value="oauth">
            <ExternalLink className="h-4 w-4 mr-2" />
            OAuth
          </TabsTrigger>
          <TabsTrigger value="inspector">
            <Database className="h-4 w-4 mr-2" />
            Inspector
          </TabsTrigger>
        </TabsList>

        <TabsContent value="overview" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle className="flex items-center justify-between">
                  <span className="flex items-center gap-2">
                    <Server className="h-5 w-5" />
                    Local Servers
                  </span>
                  <Dialog open={showAddLocalDialog} onOpenChange={setShowAddLocalDialog}>
                    <DialogTrigger asChild>
                      <Button size="sm">
                        <Plus className="h-4 w-4 mr-2" />
                        Add
                      </Button>
                    </DialogTrigger>
                    <DialogContent>
                      <DialogHeader>
                        <DialogTitle>Start Local MCP Server</DialogTitle>
                        <DialogDescription>
                          Create a new local MCP server instance
                        </DialogDescription>
                      </DialogHeader>
                      
                      <div className="space-y-4">
                        <div>
                          <Label htmlFor="local-name">Server Name</Label>
                          <Input
                            id="local-name"
                            value={newLocalName}
                            onChange={(e) => setNewLocalName(e.target.value)}
                            placeholder="My Local Server"
                          />
                        </div>
                        
                        <div>
                          <Label htmlFor="local-port">Port</Label>
                          <Input
                            id="local-port"
                            type="number"
                            value={newLocalPort}
                            onChange={(e) => setNewLocalPort(e.target.value)}
                            placeholder="8766"
                          />
                        </div>
                        
                        <div className="flex justify-end gap-2">
                          <Button variant="outline" onClick={() => setShowAddLocalDialog(false)}>
                            Cancel
                          </Button>
                          <Button onClick={startLocalServer} disabled={loading}>
                            Start Server
                          </Button>
                        </div>
                      </div>
                    </DialogContent>
                  </Dialog>
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {localServers.slice(0, 3).map((server) => (
                    <div key={server.id} className="flex items-center justify-between p-2 bg-gray-50 rounded">
                      <div className="flex items-center gap-2">
                        {getStatusIcon(server.status)}
                        <span className="font-medium">{server.name || server.id}</span>
                      </div>
                      <Badge variant="outline" className={getStatusColor(server.status)}>
                        {server.status}
                      </Badge>
                    </div>
                  ))}
                  {localServers.length === 0 && (
                    <div className="text-center py-4 text-muted-foreground">
                      No local servers running
                    </div>
                  )}
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle className="flex items-center justify-between">
                  <span className="flex items-center gap-2">
                    <Globe className="h-5 w-5" />
                    Remote Servers
                  </span>
                  <div className="flex gap-2">
                    <Button 
                      size="sm" 
                      variant="outline"
                      onClick={() => setShowSeedServersDialog(true)}
                    >
                      <Database className="h-4 w-4 mr-2" />
                      Seed List
                    </Button>
                    <Dialog open={showAddRemoteDialog} onOpenChange={setShowAddRemoteDialog}>
                      <DialogTrigger asChild>
                        <Button size="sm">
                          <Plus className="h-4 w-4 mr-2" />
                          Add
                        </Button>
                      </DialogTrigger>
                    <DialogContent>
                      <DialogHeader>
                        <DialogTitle>Connect to Remote MCP Server</DialogTitle>
                        <DialogDescription>
                          Add a connection to an external MCP server
                        </DialogDescription>
                      </DialogHeader>
                      
                      <div className="space-y-4">
                        <div>
                          <Label htmlFor="remote-name">Server Name</Label>
                          <Input
                            id="remote-name"
                            value={newRemoteName}
                            onChange={(e) => setNewRemoteName(e.target.value)}
                            placeholder="Remote MCP Server"
                          />
                        </div>
                        
                        <div>
                          <Label htmlFor="remote-url">WebSocket URL</Label>
                          <Input
                            id="remote-url"
                            value={newRemoteUrl}
                            onChange={(e) => setNewRemoteUrl(e.target.value)}
                            placeholder="ws://localhost:8765/mcp"
                          />
                          <p className="text-sm text-muted-foreground mt-1">
                            Enter the WebSocket URL of your MCP server (e.g., ws://localhost:3000/mcp)
                          </p>
                        </div>
                        
                        <div className="flex justify-between items-center">
                          <Button 
                            variant="outline" 
                            onClick={() => testConnection(newRemoteUrl)}
                            disabled={!newRemoteUrl || loading}
                            size="sm"
                          >
                            Test URL
                          </Button>
                          <div className="flex gap-2">
                            <Button variant="outline" onClick={() => setShowAddRemoteDialog(false)}>
                              Cancel
                            </Button>
                            <Button onClick={addRemoteServer} disabled={!newRemoteName || !newRemoteUrl || loading}>
                              Add Server
                            </Button>
                          </div>
                        </div>
                      </div>
                    </DialogContent>
                  </Dialog>
                  </div>
                </CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {remoteServers.slice(0, 3).map((server) => (
                    <div key={server.id} className="flex items-center justify-between p-2 bg-gray-50 rounded">
                      <div className="flex items-center gap-2">
                        {getStatusIcon(server.status)}
                        <span className="font-medium">{server.name || server.id}</span>
                      </div>
                      <Badge variant="outline" className={getStatusColor(server.status)}>
                        {server.status}
                      </Badge>
                    </div>
                  ))}
                  {remoteServers.length === 0 && (
                    <div className="text-center py-4 text-muted-foreground">
                      No remote servers connected
                    </div>
                  )}
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="local" className="space-y-4">
          <LocalServersPanel 
            servers={localServers}
            onStopServer={stopLocalServer}
            onRefresh={loadLocalServers}
            loading={loading}
          />
        </TabsContent>

        <TabsContent value="remote" className="space-y-4">
          <RemoteServersPanel 
            servers={remoteServers}
            onConnect={connectRemoteServer}
            onDisconnect={disconnectRemoteServer}
            onRefresh={loadRemoteServers}
            loading={loading}
          />
        </TabsContent>

        <TabsContent value="oauth" className="space-y-4">
          <MCPOAuthManager 
            serverId="default"
            serverName="Default OAuth"
            authType="oauth2"
            onAuthChange={(provider, authenticated) => {
              console.log(`OAuth ${authenticated ? 'connected' : 'disconnected'} for ${provider}`);
              // Refresh server data when OAuth status changes
              loadRemoteServers();
            }}
          />
        </TabsContent>

        <TabsContent value="inspector" className="space-y-4">
          <InspectorCompatibilityPanel 
            localServers={localServers}
            remoteServers={remoteServers}
            inspectorConfig={inspectorConfig}
            onConfigUpdate={setInspectorConfig}
            loading={loading}
          />
        </TabsContent>
      </Tabs>

      {/* Discovery Dialog */}
      <Dialog open={showDiscoveryDialog} onOpenChange={setShowDiscoveryDialog}>
        <DialogContent>
          <DialogHeader>
            <DialogTitle>Discovered MCP Servers</DialogTitle>
            <DialogDescription>
              Found {discoveredServers.length} server(s) on the network
            </DialogDescription>
          </DialogHeader>
          
          <div className="space-y-2 max-h-64 overflow-y-auto">
            {discoveredServers.map((server, index) => (
              <div key={index} className="flex items-center justify-between p-3 border rounded">
                <div>
                  <div className="font-medium">{server.type} server</div>
                  <div className="text-sm text-muted-foreground">{server.url}</div>
                </div>
                <Button 
                  size="sm" 
                  onClick={() => connectDiscoveredServer(server)}
                  disabled={loading}
                >
                  Connect
                </Button>
              </div>
            ))}
            {discoveredServers.length === 0 && (
              <div className="text-center py-8 text-muted-foreground">
                No servers discovered
              </div>
            )}
          </div>
        </DialogContent>
      </Dialog>

      {/* Seed Servers Dialog */}
      <SeedServersDialog
        open={showSeedServersDialog}
        onOpenChange={setShowSeedServersDialog}
        onServerAdded={loadRemoteServers}
      />
    </div>
  )
}

interface LocalServersPanelProps {
  servers: MCPServer[]
  onStopServer: (id: string) => Promise<void>
  onRefresh: () => Promise<void>
  loading: boolean
}

function LocalServersPanel({ servers, onStopServer, onRefresh: _onRefresh, loading }: LocalServersPanelProps) {
  return (
    <div className="space-y-4">
      {servers.map((server) => (
        <Card key={server.id}>
          <CardHeader>
            <CardTitle className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                {getStatusIcon(server.status)}
                <span>{server.name || server.id}</span>
              </div>
              <div className="flex gap-2">
                <Badge variant="outline" className={getStatusColor(server.status)}>
                  {server.status}
                </Badge>
                {server.port && (
                  <Badge variant="secondary">Port: {server.port}</Badge>
                )}
              </div>
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex justify-between items-center">
              <div className="text-sm text-muted-foreground">
                Local MCP Server • WebSocket Transport
              </div>
              <div className="flex gap-2">
                <Button 
                  size="sm" 
                  variant="outline"
                  onClick={() => onStopServer(server.id)}
                  disabled={loading || server.status === 'stopped'}
                >
                  <XCircle className="h-4 w-4 mr-2" />
                  Stop
                </Button>
              </div>
            </div>
          </CardContent>
        </Card>
      ))}
      
      {servers.length === 0 && (
        <Card>
          <CardContent className="flex items-center justify-center h-32">
            <div className="text-center text-muted-foreground">
              <Server className="h-8 w-8 mx-auto mb-2" />
              <p>No local servers running</p>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  )
}

interface RemoteServersPanelProps {
  servers: MCPServer[]
  onConnect: (id: string) => Promise<void>
  onDisconnect: (id: string) => Promise<void>
  onRefresh: () => Promise<void>
  loading: boolean
}

function RemoteServersPanel({ servers, onConnect, onDisconnect, onRefresh: _onRefresh, loading }: RemoteServersPanelProps) {
  return (
    <div className="space-y-4">
      {servers.map((server) => (
        <Card key={server.id}>
          <CardHeader>
            <CardTitle className="flex items-center justify-between">
              <div className="flex items-center gap-2">
                {getStatusIcon(server.status)}
                <span>{server.name || server.id}</span>
              </div>
              <Badge variant="outline" className={getStatusColor(server.status)}>
                {server.status}
              </Badge>
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="flex justify-between items-center">
              <div className="text-sm text-muted-foreground">
                {server.url}
              </div>
              <div className="flex gap-2">
                {server.status === 'connected' ? (
                  <Button 
                    size="sm" 
                    variant="outline"
                    onClick={() => onDisconnect(server.id)}
                    disabled={loading}
                  >
                    <XCircle className="h-4 w-4 mr-2" />
                    Disconnect
                  </Button>
                ) : (
                  <Button 
                    size="sm" 
                    onClick={() => onConnect(server.id)}
                    disabled={loading}
                  >
                    <CheckCircle className="h-4 w-4 mr-2" />
                    Connect
                  </Button>
                )}
              </div>
            </div>
          </CardContent>
        </Card>
      ))}
      
      {servers.length === 0 && (
        <Card>
          <CardContent className="flex items-center justify-center h-32">
            <div className="text-center text-muted-foreground">
              <Globe className="h-8 w-8 mx-auto mb-2" />
              <p>No remote servers configured</p>
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  )
}

interface InspectorCompatibilityPanelProps {
  localServers: MCPServer[]
  remoteServers: MCPServer[]
  inspectorConfig: any
  onConfigUpdate: (config: any) => void
  loading: boolean
}

function InspectorCompatibilityPanel({ 
  localServers, 
  remoteServers, 
  inspectorConfig, 
  onConfigUpdate, 
  loading 
}: InspectorCompatibilityPanelProps) {
  const [_exportedConfig, _setExportedConfig] = useState('')
  const [selectedServerId, setSelectedServerId] = useState('')

  const allServers = [...localServers, ...remoteServers]

  const exportServerEntry = async (serverId: string) => {
    try {
      const response = await fetch(`/api/mcp/export/server/${serverId}`)
      if (response.ok) {
        const config = await response.text()
        await navigator.clipboard.writeText(config)
        alert('Server configuration copied to clipboard!')
      }
    } catch (error) {
      console.error('Failed to export server entry:', error)
      alert('Failed to export server configuration')
    }
  }

  const exportServersFile = async () => {
    try {
      const response = await fetch('/api/mcp/export/servers')
      if (response.ok) {
        const config = await response.text()
        await navigator.clipboard.writeText(config)
        alert('Complete servers file copied to clipboard!')
      }
    } catch (error) {
      console.error('Failed to export servers file:', error)
      alert('Failed to export servers file')
    }
  }

  const downloadServersFile = async () => {
    try {
      const response = await fetch('/api/mcp/export/servers')
      if (response.ok) {
        const config = await response.text()
        const blob = new Blob([config], { type: 'application/json' })
        const url = URL.createObjectURL(blob)
        const a = document.createElement('a')
        a.href = url
        a.download = 'mcp.json'
        document.body.appendChild(a)
        a.click()
        document.body.removeChild(a)
        URL.revokeObjectURL(url)
      }
    } catch (error) {
      console.error('Failed to download servers file:', error)
      alert('Failed to download servers file')
    }
  }

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <ExternalLink className="h-5 w-5" />
            MCP Inspector Compatibility
          </CardTitle>
          <CardDescription>
            Export server configurations for use with the official MCP Inspector tool
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <Button 
              onClick={exportServersFile}
              disabled={loading || allServers.length === 0}
              className="flex items-center gap-2"
            >
              <Copy className="h-4 w-4" />
              Copy Servers File
            </Button>
            <Button 
              onClick={downloadServersFile}
              disabled={loading || allServers.length === 0}
              variant="outline"
              className="flex items-center gap-2"
            >
              <Download className="h-4 w-4" />
              Download mcp.json
            </Button>
            <Button 
              onClick={() => window.open('https://github.com/modelcontextprotocol/inspector', '_blank')}
              variant="outline"
              className="flex items-center gap-2"
            >
              <ExternalLink className="h-4 w-4" />
              Open Inspector
            </Button>
          </div>

          <div className="border-t pt-4">
            <h4 className="font-medium mb-2">Individual Server Export</h4>
            <div className="flex gap-2">
              <select 
                value={selectedServerId}
                onChange={(e) => setSelectedServerId(e.target.value)}
                className="flex-1 px-3 py-2 border rounded-md"
                disabled={loading || allServers.length === 0}
              >
                <option value="">Select a server...</option>
                {allServers.map((server) => (
                  <option key={server.id} value={server.id}>
                    {server.name || server.id} ({server.type})
                  </option>
                ))}
              </select>
              <Button 
                onClick={() => exportServerEntry(selectedServerId)}
                disabled={loading || !selectedServerId}
                variant="outline"
              >
                <Copy className="h-4 w-4 mr-2" />
                Copy Config
              </Button>
            </div>
          </div>

          <div className="bg-blue-50 p-4 rounded-lg">
            <h4 className="font-medium text-blue-900 mb-2">Using with MCP Inspector</h4>
            <div className="text-sm text-blue-800 space-y-2">
              <p>1. Copy or download the complete servers file (mcp.json)</p>
              <p>2. Run: <code className="bg-blue-100 px-1 rounded">npx @modelcontextprotocol/inspector --config mcp.json --server your-server-name</code></p>
              <p>3. Or use individual server configs with the Copy Config button above</p>
            </div>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader>
          <CardTitle>Inspector Configuration</CardTitle>
          <CardDescription>
            Configure timeouts and settings for MCP Inspector compatibility
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
            <div>
              <Label htmlFor="request-timeout">Request Timeout (ms)</Label>
              <Input
                id="request-timeout"
                type="number"
                value={inspectorConfig.mcp_server_request_timeout || 10000}
                onChange={(e) => onConfigUpdate({
                  ...inspectorConfig,
                  mcp_server_request_timeout: parseInt(e.target.value)
                })}
              />
            </div>
            <div>
              <Label htmlFor="max-timeout">Max Total Timeout (ms)</Label>
              <Input
                id="max-timeout"
                type="number"
                value={inspectorConfig.mcp_request_max_total_timeout || 60000}
                onChange={(e) => onConfigUpdate({
                  ...inspectorConfig,
                  mcp_request_max_total_timeout: parseInt(e.target.value)
                })}
              />
            </div>
          </div>
        </CardContent>
      </Card>
    </div>
  )
}

function getStatusIcon(status: string) {
  switch (status) {
    case 'connected':
    case 'running':
      return <CheckCircle className="h-4 w-4 text-green-500" />
    case 'disconnected':
    case 'stopped':
      return <XCircle className="h-4 w-4 text-gray-500" />
    case 'error':
      return <AlertCircle className="h-4 w-4 text-red-500" />
    default:
      return <AlertCircle className="h-4 w-4 text-yellow-500" />
  }
}

function getStatusColor(status: string) {
  switch (status) {
    case 'connected':
    case 'running':
      return 'text-green-600 bg-green-50'
    case 'disconnected':
    case 'stopped':
      return 'text-gray-600 bg-gray-50'
    case 'error':
      return 'text-red-600 bg-red-50'
    default:
      return 'text-yellow-600 bg-yellow-50'
  }
}