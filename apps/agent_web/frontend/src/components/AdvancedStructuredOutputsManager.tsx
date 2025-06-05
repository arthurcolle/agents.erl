import React, { useState, useEffect, useCallback } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from './ui/card';
import { Button } from './ui/button';
import { Badge } from './ui/badge';
import { Input } from './ui/input';
import { Label } from './ui/label';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from './ui/select';
import { Textarea } from './ui/textarea';
import { Tabs, TabsContent, TabsList, TabsTrigger } from './ui/tabs';
import { Progress } from './ui/progress';
import { Alert, AlertDescription } from './ui/alert';
import { 
  RefreshCw, 
  Search, 
  Settings, 
  Play, 
  Database, 
  Network, 
  Terminal,
  GitBranch,
  Clock,
  CheckCircle,
  AlertCircle,
  Code,
  Zap,
  Filter,
  Download,
  Upload,
  ArrowRight,
  Cpu,
  Globe
} from 'lucide-react';

interface MCPServer {
  id: string;
  name: string;
  url: string;
  description: string;
  category: 'reference' | 'official' | 'community';
  parsed_at: number;
  status?: 'connected' | 'disconnected' | 'error';
}

interface RegistryStats {
  last_update: number;
  total_servers: number;
  update_interval: number;
  registry_data: Record<string, MCPServer>;
}

interface StructuredOutput {
  id: string;
  name: string;
  description: string;
  schema: object;
  template: string;
  created_at: number;
  updated_at: number;
  usage_count: number;
}

const AdvancedStructuredOutputsManager: React.FC = () => {
  const [mcpServers, setMcpServers] = useState<MCPServer[]>([]);
  const [registryStats, setRegistryStats] = useState<RegistryStats | null>(null);
  const [selectedServers, setSelectedServers] = useState<string[]>([]);
  const [structuredOutputs, setStructuredOutputs] = useState<StructuredOutput[]>([]);
  const [activeTab, setActiveTab] = useState('discovery');
  const [searchTerm, setSearchTerm] = useState('');
  const [selectedCategory, setSelectedCategory] = useState<string>('all');
  const [isLoading, setIsLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);

  // Load MCP servers from GitHub registry
  const loadMCPServers = useCallback(async () => {
    setIsLoading(true);
    setError(null);
    
    try {
      const response = await fetch('/api/mcp-registry');
      if (!response.ok) {
        throw new Error(`HTTP ${response.status}: ${response.statusText}`);
      }
      
      const data = await response.json();
      setMcpServers(data.servers || []);
      setLastUpdate(new Date(data.generated_at * 1000));
    } catch (err) {
      setError(err instanceof Error ? err.message : 'Failed to load MCP servers');
    } finally {
      setIsLoading(false);
    }
  }, []);

  // Load registry statistics
  const loadRegistryStats = useCallback(async () => {
    try {
      const response = await fetch('/api/mcp-registry/stats');
      if (response.ok) {
        const stats = await response.json();
        setRegistryStats(stats);
      }
    } catch (err) {
      console.error('Failed to load registry stats:', err);
    }
  }, []);

  // Force registry update
  const forceRegistryUpdate = useCallback(async () => {
    setIsLoading(true);
    try {
      const response = await fetch('/api/mcp-registry', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'force_update' })
      });
      
      if (response.ok) {
        // Wait a moment then reload data
        setTimeout(() => {
          loadMCPServers();
          loadRegistryStats();
        }, 2000);
      }
    } catch (err) {
      setError('Failed to force registry update');
    } finally {
      setIsLoading(false);
    }
  }, [loadMCPServers, loadRegistryStats]);

  // Search and filter servers
  const filteredServers = mcpServers.filter(server => {
    const matchesSearch = server.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
                         server.description.toLowerCase().includes(searchTerm.toLowerCase());
    const matchesCategory = selectedCategory === 'all' || server.category === selectedCategory;
    return matchesSearch && matchesCategory;
  });

  // Generate structured output schema from selected servers
  const generateStructuredOutput = useCallback(async () => {
    if (selectedServers.length === 0) {
      setError('Please select at least one MCP server');
      return;
    }

    setIsLoading(true);
    try {
      // Create a structured output template based on selected servers
      const outputSchema = {
        type: 'object',
        properties: {
          analysis: {
            type: 'object',
            properties: {
              servers_analyzed: {
                type: 'array',
                items: { type: 'string' }
              },
              capabilities: {
                type: 'array',
                items: {
                  type: 'object',
                  properties: {
                    server: { type: 'string' },
                    tools: { type: 'array', items: { type: 'string' } },
                    resources: { type: 'array', items: { type: 'string' } },
                    category: { type: 'string' }
                  }
                }
              },
              recommendations: {
                type: 'array',
                items: {
                  type: 'object',
                  properties: {
                    action: { type: 'string' },
                    reason: { type: 'string' },
                    priority: { type: 'string', enum: ['high', 'medium', 'low'] }
                  }
                }
              }
            }
          },
          execution_plan: {
            type: 'object',
            properties: {
              steps: {
                type: 'array',
                items: {
                  type: 'object',
                  properties: {
                    step: { type: 'number' },
                    description: { type: 'string' },
                    server: { type: 'string' },
                    tool: { type: 'string' },
                    expected_output: { type: 'string' }
                  }
                }
              },
              estimated_duration: { type: 'string' },
              complexity: { type: 'string', enum: ['low', 'medium', 'high'] }
            }
          }
        }
      };

      const newOutput: StructuredOutput = {
        id: `output_${Date.now()}`,
        name: `MCP Analysis - ${selectedServers.length} servers`,
        description: `Structured output for analyzing ${selectedServers.join(', ')}`,
        schema: outputSchema,
        template: JSON.stringify(outputSchema, null, 2),
        created_at: Date.now(),
        updated_at: Date.now(),
        usage_count: 0
      };

      setStructuredOutputs(prev => [...prev, newOutput]);
      setActiveTab('outputs');
    } catch (err) {
      setError('Failed to generate structured output');
    } finally {
      setIsLoading(false);
    }
  }, [selectedServers]);

  useEffect(() => {
    loadMCPServers();
    loadRegistryStats();
    
    // Set up auto-refresh every 5 minutes
    const interval = setInterval(() => {
      loadRegistryStats();
    }, 5 * 60 * 1000);
    
    return () => clearInterval(interval);
  }, [loadMCPServers, loadRegistryStats]);

  const getCategoryIcon = (category: string) => {
    switch (category) {
      case 'reference': return <Database className="w-4 h-4" />;
      case 'official': return <CheckCircle className="w-4 h-4" />;
      case 'community': return <Network className="w-4 h-4" />;
      default: return <Globe className="w-4 h-4" />;
    }
  };

  const getCategoryColor = (category: string) => {
    switch (category) {
      case 'reference': return 'bg-blue-100 text-blue-800';
      case 'official': return 'bg-green-100 text-green-800';
      case 'community': return 'bg-purple-100 text-purple-800';
      default: return 'bg-gray-100 text-gray-800';
    }
  };

  return (
    <div className="w-full max-w-7xl mx-auto p-6 space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">
            Advanced Structured Outputs Manager
          </h1>
          <p className="text-gray-600 mt-2">
            Web-based MCP server discovery and structured output generation
          </p>
        </div>
        <div className="flex items-center space-x-4">
          {registryStats && (
            <div className="text-sm text-gray-500">
              {registryStats.total_servers} servers loaded
              {lastUpdate && (
                <span className="ml-2">
                  • Last updated: {lastUpdate.toLocaleTimeString()}
                </span>
              )}
            </div>
          )}
          <Button
            onClick={forceRegistryUpdate}
            disabled={isLoading}
            variant="outline"
            size="sm"
          >
            <RefreshCw className={`w-4 h-4 mr-2 ${isLoading ? 'animate-spin' : ''}`} />
            Refresh Registry
          </Button>
        </div>
      </div>

      {/* Error Alert */}
      {error && (
        <Alert variant="destructive">
          <AlertCircle className="h-4 w-4" />
          <AlertDescription>
            <strong>Error:</strong> {error}
          </AlertDescription>
        </Alert>
      )}

      {/* Main Tabs */}
      <Tabs value={activeTab} onValueChange={setActiveTab} className="w-full">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="discovery" className="flex items-center space-x-2">
            <Search className="w-4 h-4" />
            <span>Server Discovery</span>
          </TabsTrigger>
          <TabsTrigger value="composition" className="flex items-center space-x-2">
            <Code className="w-4 h-4" />
            <span>Output Composition</span>
          </TabsTrigger>
          <TabsTrigger value="outputs" className="flex items-center space-x-2">
            <Terminal className="w-4 h-4" />
            <span>Structured Outputs</span>
          </TabsTrigger>
          <TabsTrigger value="execution" className="flex items-center space-x-2">
            <Play className="w-4 h-4" />
            <span>Execution</span>
          </TabsTrigger>
        </TabsList>

        {/* Server Discovery Tab */}
        <TabsContent value="discovery" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center space-x-2">
                <Globe className="w-5 h-5" />
                <span>MCP Server Discovery</span>
              </CardTitle>
              <CardDescription>
                Discover and explore MCP servers from the official registry
              </CardDescription>
            </CardHeader>
            <CardContent>
              {/* Filters */}
              <div className="flex space-x-4 mb-6">
                <div className="flex-1">
                  <Label htmlFor="search">Search Servers</Label>
                  <Input
                    id="search"
                    placeholder="Search by name or description..."
                    value={searchTerm}
                    onChange={(e) => setSearchTerm(e.target.value)}
                    className="mt-1"
                  />
                </div>
                <div className="w-48">
                  <Label htmlFor="category">Category</Label>
                  <Select value={selectedCategory} onValueChange={setSelectedCategory}>
                    <SelectTrigger className="mt-1">
                      <SelectValue placeholder="Select category" />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All Categories</SelectItem>
                      <SelectItem value="reference">Reference</SelectItem>
                      <SelectItem value="official">Official</SelectItem>
                      <SelectItem value="community">Community</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
              </div>

              {/* Server Grid */}
              <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
                {filteredServers.map((server) => (
                  <Card
                    key={server.id}
                    className={`cursor-pointer transition-all duration-200 hover:shadow-md ${
                      selectedServers.includes(server.id) 
                        ? 'ring-2 ring-blue-500 bg-blue-50' 
                        : 'hover:bg-gray-50'
                    }`}
                    onClick={() => {
                      setSelectedServers(prev => 
                        prev.includes(server.id)
                          ? prev.filter(id => id !== server.id)
                          : [...prev, server.id]
                      );
                    }}
                  >
                    <CardHeader className="pb-3">
                      <div className="flex items-start justify-between">
                        <CardTitle className="text-sm font-medium truncate">
                          {server.name}
                        </CardTitle>
                        <Badge 
                          variant="secondary" 
                          className={`ml-2 ${getCategoryColor(server.category)}`}
                        >
                          <div className="flex items-center space-x-1">
                            {getCategoryIcon(server.category)}
                            <span className="capitalize">{server.category}</span>
                          </div>
                        </Badge>
                      </div>
                    </CardHeader>
                    <CardContent className="pt-0">
                      <p className="text-xs text-gray-600 line-clamp-3">
                        {server.description}
                      </p>
                      {server.url && (
                        <div className="mt-2 text-xs text-blue-600 truncate">
                          {server.url}
                        </div>
                      )}
                    </CardContent>
                  </Card>
                ))}
              </div>

              {filteredServers.length === 0 && !isLoading && (
                <div className="text-center text-gray-500 py-8">
                  No servers found matching your criteria
                </div>
              )}

              {isLoading && (
                <div className="text-center py-8">
                  <div className="animate-spin rounded-full h-8 w-8 border-b-2 border-blue-600 mx-auto"></div>
                  <p className="text-gray-500 mt-2">Loading MCP servers...</p>
                </div>
              )}
            </CardContent>
          </Card>
        </TabsContent>

        {/* Output Composition Tab */}
        <TabsContent value="composition" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center space-x-2">
                <Code className="w-5 h-5" />
                <span>Structured Output Composition</span>
              </CardTitle>
              <CardDescription>
                Generate structured output schemas from selected MCP servers
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-6">
                {/* Selected Servers */}
                <div>
                  <Label className="text-sm font-medium">Selected Servers ({selectedServers.length})</Label>
                  <div className="mt-2 flex flex-wrap gap-2">
                    {selectedServers.map((serverId) => {
                      const server = mcpServers.find(s => s.id === serverId);
                      return server ? (
                        <Badge
                          key={serverId}
                          variant="outline"
                          className="flex items-center space-x-1"
                        >
                          {getCategoryIcon(server.category)}
                          <span>{server.name}</span>
                          <button
                            onClick={() => setSelectedServers(prev => prev.filter(id => id !== serverId))}
                            className="ml-1 text-gray-400 hover:text-gray-600"
                          >
                            ×
                          </button>
                        </Badge>
                      ) : null;
                    })}
                  </div>
                  {selectedServers.length === 0 && (
                    <p className="text-sm text-gray-500 mt-2">
                      No servers selected. Go to Server Discovery to select servers.
                    </p>
                  )}
                </div>

                {/* Action Buttons */}
                <div className="flex space-x-4">
                  <Button
                    onClick={generateStructuredOutput}
                    disabled={selectedServers.length === 0 || isLoading}
                    className="flex items-center space-x-2"
                  >
                    <Zap className="w-4 h-4" />
                    <span>Generate Structured Output</span>
                  </Button>
                  <Button
                    variant="outline"
                    onClick={() => setSelectedServers([])}
                    disabled={selectedServers.length === 0}
                  >
                    Clear Selection
                  </Button>
                </div>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Structured Outputs Tab */}
        <TabsContent value="outputs" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center space-x-2">
                <Terminal className="w-5 h-5" />
                <span>Generated Structured Outputs</span>
              </CardTitle>
              <CardDescription>
                Manage and execute your structured output templates
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4">
                {structuredOutputs.map((output) => (
                  <Card key={output.id} className="border-l-4 border-l-blue-500">
                    <CardHeader className="pb-3">
                      <div className="flex items-start justify-between">
                        <div>
                          <CardTitle className="text-lg">{output.name}</CardTitle>
                          <CardDescription>{output.description}</CardDescription>
                        </div>
                        <div className="flex items-center space-x-2">
                          <Badge variant="outline" className="text-xs">
                            <Clock className="w-3 h-3 mr-1" />
                            {new Date(output.created_at).toLocaleDateString()}
                          </Badge>
                          <Badge variant="secondary" className="text-xs">
                            Used {output.usage_count} times
                          </Badge>
                        </div>
                      </div>
                    </CardHeader>
                    <CardContent>
                      <div className="space-y-4">
                        <div>
                          <Label className="text-sm font-medium">Schema Preview</Label>
                          <pre className="mt-1 p-3 bg-gray-100 rounded-md text-xs overflow-x-auto">
                            {output.template}
                          </pre>
                        </div>
                        <div className="flex space-x-2">
                          <Button size="sm" variant="outline">
                            <Download className="w-4 h-4 mr-2" />
                            Export
                          </Button>
                          <Button size="sm" variant="outline">
                            <Code className="w-4 h-4 mr-2" />
                            Edit
                          </Button>
                          <Button size="sm">
                            <Play className="w-4 h-4 mr-2" />
                            Execute
                          </Button>
                        </div>
                      </div>
                    </CardContent>
                  </Card>
                ))}

                {structuredOutputs.length === 0 && (
                  <div className="text-center text-gray-500 py-8">
                    <Terminal className="w-12 h-12 mx-auto text-gray-300 mb-4" />
                    <p>No structured outputs generated yet</p>
                    <p className="text-sm">Use the Output Composition tab to create your first structured output</p>
                  </div>
                )}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Execution Tab */}
        <TabsContent value="execution" className="space-y-6">
          <Card>
            <CardHeader>
              <CardTitle className="flex items-center space-x-2">
                <Cpu className="w-5 h-5" />
                <span>Advanced Execution Engine</span>
              </CardTitle>
              <CardDescription>
                Execute structured outputs with real-time monitoring and decomposition
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="text-center text-gray-500 py-8">
                <Cpu className="w-12 h-12 mx-auto text-gray-300 mb-4" />
                <p>Execution engine coming soon</p>
                <p className="text-sm">Advanced execution capabilities with real-time monitoring</p>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default AdvancedStructuredOutputsManager;