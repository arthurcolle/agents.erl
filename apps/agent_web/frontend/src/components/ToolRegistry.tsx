import { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { ScrollArea } from '@/components/ui/scroll-area'
import { 
  Wrench as Tool, 
  Search, 
  Filter, 
  Plus, 
  Settings, 
  Play, 
  Code, 
  Database, 
  Network,
  Download,
  Upload,
  RefreshCw,
  CheckCircle,
  XCircle,
  AlertCircle,
  Brain,
  Star,
  Users,
  Globe
} from 'lucide-react'
import { cn } from '@/lib/utils'

interface ToolDefinition {
  id: string
  name: string
  description: string
  category: 'system' | 'ai' | 'data' | 'network' | 'utility' | 'custom'
  provider: 'builtin' | 'mcp' | 'custom' | 'community'
  version: string
  status: 'available' | 'installed' | 'error' | 'deprecated'
  parameters: {
    name: string
    type: string
    required: boolean
    description: string
    default?: any
  }[]
  usage_count: number
  last_used?: Date
  documentation_url?: string
  source_url?: string
  icon?: string
  tags: string[]
  rating?: number
  downloads?: number
}

interface MCPServer {
  id: string
  name: string
  description: string
  url: string
  status: 'connected' | 'disconnected' | 'error'
  tools: ToolDefinition[]
  version: string
  author: string
  category: 'official' | 'community' | 'verified'
  last_updated: Date
}

export default function ToolRegistry() {
  const [tools, setTools] = useState<ToolDefinition[]>([])
  const [mcpServers, setMcpServers] = useState<MCPServer[]>([])
  const [searchTerm, setSearchTerm] = useState('')
  const [selectedCategory, setSelectedCategory] = useState<string>('all')
  const [selectedProvider, setSelectedProvider] = useState<string>('all')
  const [isLoading, setIsLoading] = useState(false)
  const [activeTab, setActiveTab] = useState('tools')

  // Load tools and MCP servers
  useEffect(() => {
    loadTools()
    loadMcpServers()
  }, [])

  const loadTools = async () => {
    setIsLoading(true)
    try {
      const response = await fetch('/api/tools/registry')
      if (response.ok) {
        const data = await response.json()
        setTools(data.tools || [])
      } else {
        // Load mock data
        loadMockTools()
      }
    } catch (error) {
      console.error('Failed to load tools:', error)
      loadMockTools()
    } finally {
      setIsLoading(false)
    }
  }

  const loadMockTools = () => {
    const mockTools: ToolDefinition[] = [
      {
        id: 'web_search',
        name: 'Web Search',
        description: 'Search the web for real-time information using multiple search engines',
        category: 'network',
        provider: 'builtin',
        version: '1.2.0',
        status: 'available',
        parameters: [
          { name: 'query', type: 'string', required: true, description: 'Search query' },
          { name: 'limit', type: 'number', required: false, description: 'Number of results', default: 10 }
        ],
        usage_count: 1247,
        last_used: new Date(Date.now() - 1000 * 60 * 30),
        tags: ['search', 'web', 'information'],
        rating: 4.8,
        downloads: 15420
      },
      {
        id: 'code_interpreter',
        name: 'Code Interpreter',
        description: 'Execute Python code safely in a sandboxed environment',
        category: 'ai',
        provider: 'builtin',
        version: '2.1.0',
        status: 'available',
        parameters: [
          { name: 'code', type: 'string', required: true, description: 'Python code to execute' },
          { name: 'timeout', type: 'number', required: false, description: 'Execution timeout', default: 30 }
        ],
        usage_count: 892,
        last_used: new Date(Date.now() - 1000 * 60 * 10),
        tags: ['python', 'code', 'execution'],
        rating: 4.9,
        downloads: 8904
      },
      {
        id: 'file_manager',
        name: 'File Manager',
        description: 'Read, write, and manage files with secure access controls',
        category: 'system',
        provider: 'builtin',
        version: '1.0.5',
        status: 'available',
        parameters: [
          { name: 'operation', type: 'string', required: true, description: 'Operation type: read|write|list|delete' },
          { name: 'path', type: 'string', required: true, description: 'File or directory path' }
        ],
        usage_count: 634,
        last_used: new Date(Date.now() - 1000 * 60 * 60 * 2),
        tags: ['files', 'filesystem', 'io'],
        rating: 4.6,
        downloads: 12300
      },
      {
        id: 'database_query',
        name: 'Database Query',
        description: 'Query various databases with SQL and NoSQL support',
        category: 'data',
        provider: 'mcp',
        version: '1.4.2',
        status: 'installed',
        parameters: [
          { name: 'query', type: 'string', required: true, description: 'Database query' },
          { name: 'database', type: 'string', required: true, description: 'Database connection name' }
        ],
        usage_count: 445,
        last_used: new Date(Date.now() - 1000 * 60 * 60 * 6),
        tags: ['database', 'sql', 'query'],
        rating: 4.7,
        downloads: 7891
      },
      {
        id: 'image_generation',
        name: 'Image Generation',
        description: 'Generate images using AI models (DALL-E, Midjourney, etc.)',
        category: 'ai',
        provider: 'community',
        version: '0.8.1',
        status: 'available',
        parameters: [
          { name: 'prompt', type: 'string', required: true, description: 'Image generation prompt' },
          { name: 'model', type: 'string', required: false, description: 'Model to use', default: 'dall-e-3' }
        ],
        usage_count: 278,
        last_used: new Date(Date.now() - 1000 * 60 * 60 * 12),
        tags: ['image', 'ai', 'generation'],
        rating: 4.4,
        downloads: 5632
      }
    ]
    setTools(mockTools)
  }

  const loadMcpServers = async () => {
    try {
      const response = await fetch('/api/mcp/servers')
      if (response.ok) {
        const data = await response.json()
        setMcpServers(data.servers || [])
      } else {
        // Load mock MCP servers
        loadMockMcpServers()
      }
    } catch (error) {
      console.error('Failed to load MCP servers:', error)
      loadMockMcpServers()
    }
  }

  const loadMockMcpServers = () => {
    const mockServers: MCPServer[] = [
      {
        id: 'github_mcp',
        name: 'GitHub MCP Server',
        description: 'Interact with GitHub repositories, issues, and pull requests',
        url: 'https://github.com/modelcontextprotocol/servers/tree/main/src/github',
        status: 'connected',
        tools: [],
        version: '0.1.0',
        author: 'Anthropic',
        category: 'official',
        last_updated: new Date(Date.now() - 1000 * 60 * 60 * 24 * 2)
      },
      {
        id: 'postgres_mcp',
        name: 'PostgreSQL MCP Server',
        description: 'Query and manage PostgreSQL databases',
        url: 'https://github.com/modelcontextprotocol/servers/tree/main/src/postgres',
        status: 'connected',
        tools: [],
        version: '0.2.1',
        author: 'Anthropic',
        category: 'official',
        last_updated: new Date(Date.now() - 1000 * 60 * 60 * 24 * 1)
      },
      {
        id: 'slack_mcp',
        name: 'Slack MCP Server',
        description: 'Send messages and interact with Slack workspaces',
        url: 'https://github.com/modelcontextprotocol/servers/tree/main/src/slack',
        status: 'disconnected',
        tools: [],
        version: '0.1.5',
        author: 'Community',
        category: 'community',
        last_updated: new Date(Date.now() - 1000 * 60 * 60 * 24 * 7)
      }
    ]
    setMcpServers(mockServers)
  }

  // Filter tools
  const filteredTools = tools.filter(tool => {
    const matchesSearch = tool.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         tool.description.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         tool.tags.some(tag => tag.toLowerCase().includes(searchTerm.toLowerCase()))
    
    const matchesCategory = selectedCategory === 'all' || tool.category === selectedCategory
    const matchesProvider = selectedProvider === 'all' || tool.provider === selectedProvider
    
    return matchesSearch && matchesCategory && matchesProvider
  })

  // Install/uninstall tool
  const toggleToolInstallation = async (toolId: string) => {
    const tool = tools.find(t => t.id === toolId)
    if (!tool) return

    try {
      const action = tool.status === 'installed' ? 'uninstall' : 'install'
      const response = await fetch(`/api/tools/${toolId}/${action}`, {
        method: 'POST'
      })

      if (response.ok) {
        setTools(prev => prev.map(t => 
          t.id === toolId 
            ? { ...t, status: action === 'install' ? 'installed' : 'available' }
            : t
        ))
      }
    } catch (error) {
      console.error(`Failed to ${tool.status === 'installed' ? 'uninstall' : 'install'} tool:`, error)
    }
  }

  // Test tool
  const testTool = async (toolId: string) => {
    try {
      const response = await fetch(`/api/tools/${toolId}/test`, {
        method: 'POST'
      })
      
      if (response.ok) {
        console.log('Tool test successful')
      }
    } catch (error) {
      console.error('Tool test failed:', error)
    }
  }

  // Connect to MCP server
  const connectMcpServer = async (serverId: string) => {
    try {
      const response = await fetch(`/api/mcp/servers/${serverId}/connect`, {
        method: 'POST'
      })

      if (response.ok) {
        setMcpServers(prev => prev.map(server =>
          server.id === serverId
            ? { ...server, status: 'connected' }
            : server
        ))
      }
    } catch (error) {
      console.error('Failed to connect to MCP server:', error)
    }
  }

  const getToolIcon = (category: string) => {
    switch (category) {
      case 'system': return <Settings className="h-4 w-4" />
      case 'ai': return <Brain className="h-4 w-4" />
      case 'data': return <Database className="h-4 w-4" />
      case 'network': return <Network className="h-4 w-4" />
      case 'utility': return <Tool className="h-4 w-4" />
      default: return <Code className="h-4 w-4" />
    }
  }

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'available': return <Download className="h-4 w-4 text-blue-500" />
      case 'installed': return <CheckCircle className="h-4 w-4 text-green-500" />
      case 'error': return <XCircle className="h-4 w-4 text-red-500" />
      case 'deprecated': return <AlertCircle className="h-4 w-4 text-yellow-500" />
      default: return <AlertCircle className="h-4 w-4 text-gray-500" />
    }
  }

  return (
    <div className="h-full flex flex-col">
      <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
        <div className="border-b p-4">
          <div className="flex items-center justify-between mb-4">
            <div>
              <h1 className="text-2xl font-bold">Tool Registry</h1>
              <p className="text-gray-600">Discover, install, and manage tools for your agents</p>
            </div>
            <div className="flex gap-2">
              <Button onClick={() => window.open('/api/tools/marketplace', '_blank')}>
                <Globe className="h-4 w-4 mr-2" />
                Marketplace
              </Button>
              <Button onClick={loadTools}>
                <RefreshCw className="h-4 w-4 mr-2" />
                Refresh
              </Button>
            </div>
          </div>

          <TabsList className="grid w-full grid-cols-3">
            <TabsTrigger value="tools">
              <Tool className="h-4 w-4 mr-2" />
              Tools ({tools.length})
            </TabsTrigger>
            <TabsTrigger value="mcp">
              <Network className="h-4 w-4 mr-2" />
              MCP Servers ({mcpServers.length})
            </TabsTrigger>
            <TabsTrigger value="marketplace">
              <Star className="h-4 w-4 mr-2" />
              Marketplace
            </TabsTrigger>
          </TabsList>
        </div>

        <div className="flex-1 overflow-hidden">
          <TabsContent value="tools" className="h-full m-0 p-4 overflow-y-auto">
            {/* Filters */}
            <div className="mb-6 space-y-3">
              <div className="flex gap-4">
                <div className="relative flex-1">
                  <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 h-4 w-4 text-gray-500" />
                  <Input
                    placeholder="Search tools..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="pl-10"
                  />
                </div>
                <select
                  value={selectedCategory}
                  onChange={(e) => setSelectedCategory(e.target.value)}
                  className="px-3 py-2 border rounded-md"
                >
                  <option value="all">All Categories</option>
                  <option value="system">System</option>
                  <option value="ai">AI</option>
                  <option value="data">Data</option>
                  <option value="network">Network</option>
                  <option value="utility">Utility</option>
                </select>
                <select
                  value={selectedProvider}
                  onChange={(e) => setSelectedProvider(e.target.value)}
                  className="px-3 py-2 border rounded-md"
                >
                  <option value="all">All Providers</option>
                  <option value="builtin">Built-in</option>
                  <option value="mcp">MCP</option>
                  <option value="community">Community</option>
                  <option value="custom">Custom</option>
                </select>
              </div>
            </div>

            {/* Tools Grid */}
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {filteredTools.map(tool => (
                <Card key={tool.id} className="hover:shadow-md transition-shadow">
                  <CardHeader className="pb-3">
                    <div className="flex items-start justify-between">
                      <div className="flex items-center gap-2">
                        {getToolIcon(tool.category)}
                        <CardTitle className="text-base">{tool.name}</CardTitle>
                      </div>
                      <div className="flex items-center gap-1">
                        {getStatusIcon(tool.status)}
                        <Badge variant="outline" className="text-xs">
                          v{tool.version}
                        </Badge>
                      </div>
                    </div>
                    <CardDescription className="text-sm">
                      {tool.description}
                    </CardDescription>
                  </CardHeader>
                  <CardContent className="space-y-3">
                    <div className="flex flex-wrap gap-1">
                      {tool.tags.map(tag => (
                        <Badge key={tag} variant="secondary" className="text-xs">
                          {tag}
                        </Badge>
                      ))}
                    </div>
                    
                    <div className="flex items-center justify-between text-xs text-gray-500">
                      <span>{tool.usage_count} uses</span>
                      {tool.rating && (
                        <div className="flex items-center gap-1">
                          <Star className="h-3 w-3 fill-yellow-400 text-yellow-400" />
                          <span>{tool.rating}</span>
                        </div>
                      )}
                    </div>

                    <div className="flex gap-2">
                      <Button
                        size="sm"
                        variant={tool.status === 'installed' ? 'destructive' : 'default'}
                        onClick={() => toggleToolInstallation(tool.id)}
                        className="flex-1"
                      >
                        {tool.status === 'installed' ? 'Uninstall' : 'Install'}
                      </Button>
                      <Button
                        size="sm"
                        variant="outline"
                        onClick={() => testTool(tool.id)}
                        disabled={tool.status !== 'installed'}
                      >
                        <Play className="h-3 w-3" />
                      </Button>
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>

            {filteredTools.length === 0 && (
              <div className="text-center py-12">
                <Tool className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                <h3 className="text-lg font-medium text-gray-700 mb-2">No tools found</h3>
                <p className="text-gray-500">Try adjusting your search or filters</p>
              </div>
            )}
          </TabsContent>

          <TabsContent value="mcp" className="h-full m-0 p-4 overflow-y-auto">
            <div className="space-y-4">
              {mcpServers.map(server => (
                <Card key={server.id}>
                  <CardHeader>
                    <div className="flex items-center justify-between">
                      <div>
                        <CardTitle className="flex items-center gap-2">
                          <Network className="h-5 w-5" />
                          {server.name}
                          <Badge variant={server.category === 'official' ? 'default' : 'secondary'}>
                            {server.category}
                          </Badge>
                        </CardTitle>
                        <CardDescription>{server.description}</CardDescription>
                      </div>
                      <div className="flex items-center gap-2">
                        <Badge 
                          variant={server.status === 'connected' ? 'default' : 'destructive'}
                          className="capitalize"
                        >
                          {server.status}
                        </Badge>
                        <Button
                          size="sm"
                          onClick={() => connectMcpServer(server.id)}
                          disabled={server.status === 'connected'}
                        >
                          {server.status === 'connected' ? 'Connected' : 'Connect'}
                        </Button>
                      </div>
                    </div>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center justify-between text-sm text-gray-600">
                      <span>Version: {server.version}</span>
                      <span>By: {server.author}</span>
                      <span>Updated: {server.last_updated.toLocaleDateString()}</span>
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>
          </TabsContent>

          <TabsContent value="marketplace" className="h-full m-0 p-4 overflow-y-auto">
            <div className="text-center py-12">
              <Globe className="h-16 w-16 text-gray-400 mx-auto mb-4" />
              <h3 className="text-xl font-medium text-gray-700 mb-2">Tool Marketplace</h3>
              <p className="text-gray-500 mb-6">Discover new tools and integrations from the community</p>
              <Button onClick={() => window.open('https://marketplace.mcp.tools', '_blank')}>
                Visit Marketplace
              </Button>
            </div>
          </TabsContent>
        </div>
      </Tabs>
    </div>
  )
}