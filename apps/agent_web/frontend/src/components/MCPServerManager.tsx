import { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  Plus, 
  Play, 
  Trash2, 
  RefreshCw, 
  CheckCircle, 
  XCircle, 
  AlertCircle,
  Settings,
  Plug
} from 'lucide-react'

interface MCPServer {
  id: string
  name: string
  url: string
  status: 'connected' | 'disconnected' | 'error'
  config?: any
  registered_at?: number
  last_seen?: number
}

interface MCPTool {
  name: string
  description?: string
  inputSchema?: any
}

export default function MCPServerManager() {
  const [servers, setServers] = useState<MCPServer[]>([])
  const [selectedServer, setSelectedServer] = useState<string | null>(null)
  const [serverTools, setServerTools] = useState<MCPTool[]>([])
  const [loading, setLoading] = useState(false)
  const [showAddDialog, setShowAddDialog] = useState(false)
  
  // Form state for adding new server
  const [newServerName, setNewServerName] = useState('')
  const [newServerUrl, setNewServerUrl] = useState('')
  const [newServerConfig, setNewServerConfig] = useState('{}')

  useEffect(() => {
    loadServers()
    const interval = setInterval(loadServers, 5000) // Refresh every 5 seconds
    return () => clearInterval(interval)
  }, [])

  useEffect(() => {
    if (selectedServer) {
      loadServerTools(selectedServer)
    }
  }, [selectedServer])

  const loadServers = async () => {
    try {
      const response = await fetch('/api/mcp/servers')
      const data = await response.json()
      setServers(data.servers || [])
    } catch (error) {
      console.error('Failed to load MCP servers:', error)
    }
  }

  const loadServerTools = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/tools`)
      const data = await response.json()
      setServerTools(data.tools || [])
    } catch (error) {
      console.error('Failed to load server tools:', error)
      setServerTools([])
    } finally {
      setLoading(false)
    }
  }

  const addServer = async () => {
    try {
      let config
      try {
        config = JSON.parse(newServerConfig)
      } catch {
        config = {}
      }

      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          name: newServerName,
          url: newServerUrl,
          config
        })
      })

      if (response.ok) {
        setShowAddDialog(false)
        setNewServerName('')
        setNewServerUrl('')
        setNewServerConfig('{}')
        await loadServers()
      }
    } catch (error) {
      console.error('Failed to add server:', error)
    }
  }

  const removeServer = async (serverId: string) => {
    try {
      const response = await fetch(`/api/mcp/servers/${serverId}`, {
        method: 'DELETE'
      })

      if (response.ok) {
        await loadServers()
        if (selectedServer === serverId) {
          setSelectedServer(null)
          setServerTools([])
        }
      }
    } catch (error) {
      console.error('Failed to remove server:', error)
    }
  }

  const testConnection = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/test`, {
        method: 'POST'
      })
      const data = await response.json()
      
      if (data.status === 'success') {
        console.log('Connection test successful')
      } else {
        console.error('Connection test failed:', data.error)
      }
      
      await loadServers()
    } catch (error) {
      console.error('Failed to test connection:', error)
    } finally {
      setLoading(false)
    }
  }

  const connectServer = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/connect`, {
        method: 'POST'
      })
      const data = await response.json()
      
      if (response.ok) {
        console.log('Server connected successfully')
        await loadServers()
      } else {
        console.error('Failed to connect server:', data.error)
      }
    } catch (error) {
      console.error('Failed to connect server:', error)
    } finally {
      setLoading(false)
    }
  }

  const disconnectServer = async (serverId: string) => {
    try {
      setLoading(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/disconnect`, {
        method: 'POST'
      })
      const data = await response.json()
      
      if (response.ok) {
        console.log('Server disconnected successfully')
        await loadServers()
        if (selectedServer === serverId) {
          setSelectedServer(null)
          setServerTools([])
        }
      } else {
        console.error('Failed to disconnect server:', data.error)
      }
    } catch (error) {
      console.error('Failed to disconnect server:', error)
    } finally {
      setLoading(false)
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'connected':
        return <CheckCircle className="h-4 w-4 text-green-500" />
      case 'disconnected':
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
        return 'text-green-600 bg-green-50'
      case 'disconnected':
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
            Manage Model Context Protocol servers and their capabilities
          </p>
        </div>
        
        <div className="flex gap-2">
          <Button variant="outline" onClick={loadServers} disabled={loading}>
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
          
          <Dialog open={showAddDialog} onOpenChange={setShowAddDialog}>
            <DialogTrigger asChild>
              <Button>
                <Plus className="h-4 w-4 mr-2" />
                Add Server
              </Button>
            </DialogTrigger>
            <DialogContent>
              <DialogHeader>
                <DialogTitle>Add MCP Server</DialogTitle>
                <DialogDescription>
                  Register a new Model Context Protocol server
                </DialogDescription>
              </DialogHeader>
              
              <div className="space-y-4">
                <div>
                  <Label htmlFor="server-name">Server Name</Label>
                  <Input
                    id="server-name"
                    value={newServerName}
                    onChange={(e) => setNewServerName(e.target.value)}
                    placeholder="My MCP Server"
                  />
                </div>
                
                <div>
                  <Label htmlFor="server-url">WebSocket URL</Label>
                  <Input
                    id="server-url"
                    value={newServerUrl}
                    onChange={(e) => setNewServerUrl(e.target.value)}
                    placeholder="ws://localhost:8000/mcp"
                  />
                </div>
                
                <div>
                  <Label htmlFor="server-config">Configuration (JSON)</Label>
                  <textarea
                    id="server-config"
                    className="w-full h-20 p-2 border border-gray-300 rounded-md"
                    value={newServerConfig}
                    onChange={(e) => setNewServerConfig(e.target.value)}
                    placeholder='{"timeout": 5000}'
                  />
                </div>
                
                <div className="flex justify-end gap-2">
                  <Button variant="outline" onClick={() => setShowAddDialog(false)}>
                    Cancel
                  </Button>
                  <Button onClick={addServer} disabled={!newServerName || !newServerUrl}>
                    Add Server
                  </Button>
                </div>
              </div>
            </DialogContent>
          </Dialog>
        </div>
      </div>

      <div className="grid grid-cols-12 gap-6">
        <div className="col-span-5">
          <Card>
            <CardHeader>
              <CardTitle>Registered Servers</CardTitle>
              <CardDescription>
                {servers.length} server{servers.length !== 1 ? 's' : ''} registered
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {servers.map((server) => (
                  <div
                    key={server.id}
                    className={`p-3 border rounded-lg cursor-pointer transition-colors ${
                      selectedServer === server.id
                        ? 'border-blue-500 bg-blue-50'
                        : 'border-gray-200 hover:border-gray-300'
                    }`}
                    onClick={() => setSelectedServer(server.id)}
                  >
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2">
                          {getStatusIcon(server.status)}
                          <h3 className="font-medium">{server.name}</h3>
                        </div>
                        <p className="text-sm text-muted-foreground mt-1">
                          {server.url}
                        </p>
                        <div className="flex items-center gap-2 mt-2">
                          <Badge variant="outline" className={getStatusColor(server.status)}>
                            {server.status}
                          </Badge>
                          <span className="text-xs text-muted-foreground">
                            ID: {server.id}
                          </span>
                        </div>
                      </div>
                      
                      <div className="flex gap-1">
                        {server.status === 'connected' ? (
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={(e) => {
                              e.stopPropagation()
                              disconnectServer(server.id)
                            }}
                            disabled={loading}
                            title="Disconnect"
                          >
                            <XCircle className="h-3 w-3 text-red-500" />
                          </Button>
                        ) : (
                          <Button
                            size="sm"
                            variant="outline"
                            onClick={(e) => {
                              e.stopPropagation()
                              connectServer(server.id)
                            }}
                            disabled={loading}
                            title="Connect"
                          >
                            <CheckCircle className="h-3 w-3 text-green-500" />
                          </Button>
                        )}
                        <Button
                          size="sm"
                          variant="outline"
                          onClick={(e) => {
                            e.stopPropagation()
                            testConnection(server.id)
                          }}
                          disabled={loading}
                          title="Test Connection"
                        >
                          <Play className="h-3 w-3" />
                        </Button>
                        <Button
                          size="sm"
                          variant="outline"
                          onClick={(e) => {
                            e.stopPropagation()
                            removeServer(server.id)
                          }}
                          title="Remove Server"
                        >
                          <Trash2 className="h-3 w-3" />
                        </Button>
                      </div>
                    </div>
                  </div>
                ))}
                
                {servers.length === 0 && (
                  <div className="text-center py-8 text-muted-foreground">
                    <Plug className="h-8 w-8 mx-auto mb-2" />
                    <p>No MCP servers registered</p>
                    <p className="text-sm">Add your first server to get started</p>
                  </div>
                )}
              </div>
            </CardContent>
          </Card>
        </div>

        <div className="col-span-7">
          {selectedServer ? (
            <MCPServerDetails 
              serverId={selectedServer} 
              tools={serverTools} 
              loading={loading}
            />
          ) : (
            <Card>
              <CardContent className="flex items-center justify-center h-64">
                <div className="text-center text-muted-foreground">
                  <Settings className="h-8 w-8 mx-auto mb-2" />
                  <p>Select a server to view details</p>
                </div>
              </CardContent>
            </Card>
          )}
        </div>
      </div>
    </div>
  )
}

interface MCPServerDetailsProps {
  serverId: string
  tools: MCPTool[]
  loading: boolean
}

function MCPServerDetails({ serverId, tools, loading }: MCPServerDetailsProps) {
  const [server, setServer] = useState<MCPServer | null>(null)
  const [resources, setResources] = useState<any[]>([])
  const [prompts, setPrompts] = useState<any[]>([])
  const [loadingResources, setLoadingResources] = useState(false)
  const [loadingPrompts, setLoadingPrompts] = useState(false)

  useEffect(() => {
    loadServerDetails()
  }, [serverId])

  const loadServerDetails = async () => {
    try {
      const response = await fetch(`/api/mcp/servers/${serverId}`)
      const data = await response.json()
      setServer(data)
    } catch (error) {
      console.error('Failed to load server details:', error)
    }
  }

  const loadResources = async () => {
    try {
      setLoadingResources(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/resources`)
      const data = await response.json()
      setResources(data.resources || [])
    } catch (error) {
      console.error('Failed to load resources:', error)
      setResources([])
    } finally {
      setLoadingResources(false)
    }
  }

  const loadPrompts = async () => {
    try {
      setLoadingPrompts(true)
      const response = await fetch(`/api/mcp/servers/${serverId}/prompts`)
      const data = await response.json()
      setPrompts(data.prompts || [])
    } catch (error) {
      console.error('Failed to load prompts:', error)
      setPrompts([])
    } finally {
      setLoadingPrompts(false)
    }
  }

  if (!server) {
    return (
      <Card>
        <CardContent className="flex items-center justify-center h-64">
          <RefreshCw className="h-6 w-6 animate-spin" />
        </CardContent>
      </Card>
    )
  }

  return (
    <Card>
      <CardHeader>
        <div className="flex items-center justify-between">
          <div>
            <CardTitle className="flex items-center gap-2">
              {getStatusIcon(server.status)}
              {server.name}
            </CardTitle>
            <CardDescription>{server.url}</CardDescription>
          </div>
          <Badge variant="outline" className={getStatusColor(server.status)}>
            {server.status}
          </Badge>
        </div>
      </CardHeader>
      
      <CardContent>
        <Tabs defaultValue="tools">
          <TabsList>
            <TabsTrigger value="tools">
              Tools ({tools.length})
            </TabsTrigger>
            <TabsTrigger value="resources">Resources</TabsTrigger>
            <TabsTrigger value="prompts">Prompts</TabsTrigger>
            <TabsTrigger value="config">Configuration</TabsTrigger>
            <TabsTrigger value="logs">Activity</TabsTrigger>
          </TabsList>
          
          <TabsContent value="tools" className="mt-4">
            {loading ? (
              <div className="flex items-center justify-center py-8">
                <RefreshCw className="h-6 w-6 animate-spin" />
              </div>
            ) : tools.length > 0 ? (
              <div className="space-y-3">
                {tools.map((tool, index) => (
                  <div key={index} className="p-3 border rounded-lg">
                    <div className="flex items-center justify-between">
                      <h4 className="font-medium">{tool.name}</h4>
                      <Button size="sm" variant="outline">
                        Test Tool
                      </Button>
                    </div>
                    {tool.description && (
                      <p className="text-sm text-muted-foreground mt-1">
                        {tool.description}
                      </p>
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8 text-muted-foreground">
                <p>No tools available from this server</p>
              </div>
            )}
          </TabsContent>
          
          <TabsContent value="resources" className="mt-4">
            {!resources.length && !loadingResources && (
              <div className="text-center py-4">
                <Button variant="outline" onClick={loadResources}>
                  Load Resources
                </Button>
              </div>
            )}
            
            {loadingResources ? (
              <div className="flex items-center justify-center py-8">
                <RefreshCw className="h-6 w-6 animate-spin" />
              </div>
            ) : resources.length > 0 ? (
              <div className="space-y-3">
                {resources.map((resource, index) => (
                  <div key={index} className="p-3 border rounded-lg">
                    <div className="flex items-center justify-between">
                      <h4 className="font-medium">{resource.name || resource.uri}</h4>
                      <Badge variant="outline">{resource.mimeType || 'unknown'}</Badge>
                    </div>
                    <p className="text-sm text-muted-foreground mt-1">
                      {resource.description || resource.uri}
                    </p>
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8 text-muted-foreground">
                <p>No resources available or failed to load</p>
              </div>
            )}
          </TabsContent>
          
          <TabsContent value="prompts" className="mt-4">
            {!prompts.length && !loadingPrompts && (
              <div className="text-center py-4">
                <Button variant="outline" onClick={loadPrompts}>
                  Load Prompts
                </Button>
              </div>
            )}
            
            {loadingPrompts ? (
              <div className="flex items-center justify-center py-8">
                <RefreshCw className="h-6 w-6 animate-spin" />
              </div>
            ) : prompts.length > 0 ? (
              <div className="space-y-3">
                {prompts.map((prompt, index) => (
                  <div key={index} className="p-3 border rounded-lg">
                    <div className="flex items-center justify-between">
                      <h4 className="font-medium">{prompt.name}</h4>
                      <Button size="sm" variant="outline">
                        Use Prompt
                      </Button>
                    </div>
                    {prompt.description && (
                      <p className="text-sm text-muted-foreground mt-1">
                        {prompt.description}
                      </p>
                    )}
                  </div>
                ))}
              </div>
            ) : (
              <div className="text-center py-8 text-muted-foreground">
                <p>No prompts available or failed to load</p>
              </div>
            )}
          </TabsContent>
          
          <TabsContent value="config" className="mt-4">
            <div className="space-y-4">
              <div>
                <Label>Server Configuration</Label>
                <pre className="mt-2 p-3 bg-gray-50 rounded-md text-sm overflow-auto">
                  {JSON.stringify(server.config || {}, null, 2)}
                </pre>
              </div>
              
              <div className="grid grid-cols-2 gap-4">
                <div>
                  <Label>Registered At</Label>
                  <p className="text-sm text-muted-foreground">
                    {server.registered_at 
                      ? new Date(server.registered_at * 1000).toLocaleString()
                      : 'Unknown'
                    }
                  </p>
                </div>
                <div>
                  <Label>Last Seen</Label>
                  <p className="text-sm text-muted-foreground">
                    {server.last_seen 
                      ? new Date(server.last_seen * 1000).toLocaleString()
                      : 'Never'
                    }
                  </p>
                </div>
              </div>
            </div>
          </TabsContent>
          
          <TabsContent value="logs" className="mt-4">
            <div className="text-center py-8 text-muted-foreground">
              <p>Activity logs will be available in a future update</p>
            </div>
          </TabsContent>
        </Tabs>
      </CardContent>
    </Card>
  )

  function getStatusIcon(status: string) {
    switch (status) {
      case 'connected':
        return <CheckCircle className="h-4 w-4 text-green-500" />
      case 'disconnected':
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
        return 'text-green-600 bg-green-50'
      case 'disconnected':
        return 'text-gray-600 bg-gray-50'
      case 'error':
        return 'text-red-600 bg-red-50'
      default:
        return 'text-yellow-600 bg-yellow-50'
    }
  }
}