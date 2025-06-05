import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Button } from "@/components/ui/button";
import { Badge } from "@/components/ui/badge";
import { Progress } from "@/components/ui/progress";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from "@/components/ui/select";
import { Activity, Network, Search, Settings, BarChart3, Eye, RefreshCw, Filter } from 'lucide-react';

interface MeshNode {
  id: string;
  type: string;
  capabilities: string[];
  resource_count: number;
  connection_count: number;
  status: 'active' | 'inactive' | 'discovering';
  last_activity: number;
}

interface MeshTopology {
  nodes: MeshNode[];
  connections: Array<{
    from: string;
    to: string;
    weight: number;
    latency: number;
  }>;
  statistics: {
    total_nodes: number;
    total_connections: number;
    average_path_length: number;
    cluster_coefficient: number;
    discovery_efficiency: number;
  };
}

interface ResourceDiscovery {
  query: string;
  resource_type: string;
  results: Array<{
    id: string;
    name: string;
    owner_agent: string;
    resource_type: string;
    access_level: string;
    discovery_path: string[];
    discovery_time_ms: number;
  }>;
  total_results: number;
  discovery_time_ms: number;
}

export default function DiscoveryMeshDashboard() {
  const [meshTopology, setMeshTopology] = useState<MeshTopology | null>(null);
  const [discoveryHistory, setDiscoveryHistory] = useState<ResourceDiscovery[]>([]);
  const [isLoading, setIsLoading] = useState(false);
  const [selectedNode, setSelectedNode] = useState<string | null>(null);
  const [discoveryQuery, setDiscoveryQuery] = useState('');
  const [discoveryResourceType, setDiscoveryResourceType] = useState('all');
  const [autoRefresh, setAutoRefresh] = useState(true);

  // Load mesh topology and statistics
  useEffect(() => {
    loadMeshTopology();
    loadDiscoveryHistory();
    
    if (autoRefresh) {
      const interval = setInterval(() => {
        loadMeshTopology();
        loadDiscoveryHistory();
      }, 5000);
      return () => clearInterval(interval);
    }
  }, [autoRefresh]);

  const loadMeshTopology = async () => {
    try {
      const response = await fetch('/api/discovery/mesh/topology');
      if (response.ok) {
        const topology = await response.json();
        setMeshTopology(topology);
      }
    } catch (error) {
      console.error('Failed to load mesh topology:', error);
    }
  };

  const loadDiscoveryHistory = async () => {
    try {
      const response = await fetch('/api/discovery/history?limit=20');
      if (response.ok) {
        const history = await response.json();
        setDiscoveryHistory(history);
      }
    } catch (error) {
      console.error('Failed to load discovery history:', error);
    }
  };

  const performDiscovery = async () => {
    if (!discoveryQuery.trim()) return;

    setIsLoading(true);
    try {
      const response = await fetch('/api/discovery/search', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          query: discoveryQuery,
          resource_type: discoveryResourceType,
          max_results: 50
        })
      });

      if (response.ok) {
        const discoveryResult = await response.json();
        setDiscoveryHistory(prev => [discoveryResult, ...prev.slice(0, 19)]);
        setDiscoveryQuery('');
      }
    } catch (error) {
      console.error('Discovery failed:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const optimizeMesh = async () => {
    setIsLoading(true);
    try {
      await fetch('/api/discovery/mesh/optimize', { method: 'POST' });
      await loadMeshTopology();
    } catch (error) {
      console.error('Mesh optimization failed:', error);
    } finally {
      setIsLoading(false);
    }
  };

  const clearDiscoveryCache = async () => {
    try {
      await fetch('/api/discovery/cache/clear', { method: 'POST' });
      await loadMeshTopology();
    } catch (error) {
      console.error('Cache clear failed:', error);
    }
  };

  const getNodeStatusColor = (status: string) => {
    switch (status) {
      case 'active': return 'bg-green-500';
      case 'inactive': return 'bg-gray-500';
      case 'discovering': return 'bg-blue-500';
      default: return 'bg-gray-500';
    }
  };

  const getResourceTypeColor = (type: string) => {
    switch (type) {
      case 'tool': return 'bg-purple-100 text-purple-800';
      case 'memory': return 'bg-blue-100 text-blue-800';
      case 'file': return 'bg-green-100 text-green-800';
      case 'conversation': return 'bg-yellow-100 text-yellow-800';
      case 'message': return 'bg-orange-100 text-orange-800';
      default: return 'bg-gray-100 text-gray-800';
    }
  };

  return (
    <div className="p-6 space-y-6">
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold tracking-tight">Discovery Mesh Dashboard</h1>
          <p className="text-muted-foreground">
            Monitor and manage cross-agent resource discovery and routing
          </p>
        </div>
        <div className="flex gap-2">
          <Button
            variant="outline"
            size="sm"
            onClick={() => setAutoRefresh(!autoRefresh)}
            className={autoRefresh ? 'bg-green-50' : ''}
          >
            <RefreshCw className={`h-4 w-4 mr-2 ${autoRefresh ? 'animate-spin' : ''}`} />
            Auto Refresh {autoRefresh ? 'On' : 'Off'}
          </Button>
          <Button variant="outline" size="sm" onClick={loadMeshTopology}>
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh
          </Button>
        </div>
      </div>

      <Tabs defaultValue="topology" className="space-y-4">
        <TabsList>
          <TabsTrigger value="topology" className="flex items-center gap-2">
            <Network className="h-4 w-4" />
            Mesh Topology
          </TabsTrigger>
          <TabsTrigger value="discovery" className="flex items-center gap-2">
            <Search className="h-4 w-4" />
            Resource Discovery
          </TabsTrigger>
          <TabsTrigger value="analytics" className="flex items-center gap-2">
            <BarChart3 className="h-4 w-4" />
            Analytics
          </TabsTrigger>
          <TabsTrigger value="settings" className="flex items-center gap-2">
            <Settings className="h-4 w-4" />
            Settings
          </TabsTrigger>
        </TabsList>

        <TabsContent value="topology" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">Active Nodes</CardTitle>
                <Network className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">
                  {meshTopology?.statistics.total_nodes || 0}
                </div>
                <p className="text-xs text-muted-foreground">
                  {meshTopology?.nodes.filter(n => n.status === 'active').length || 0} active
                </p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">Connections</CardTitle>
                <Activity className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">
                  {meshTopology?.statistics.total_connections || 0}
                </div>
                <p className="text-xs text-muted-foreground">
                  Avg path: {meshTopology?.statistics.average_path_length?.toFixed(2) || 0}
                </p>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">Discovery Efficiency</CardTitle>
                <BarChart3 className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">
                  {((meshTopology?.statistics.discovery_efficiency || 0) * 100).toFixed(1)}%
                </div>
                <Progress value={(meshTopology?.statistics.discovery_efficiency || 0) * 100} className="w-full" />
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">Cluster Coefficient</CardTitle>
                <Eye className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">
                  {(meshTopology?.statistics.cluster_coefficient || 0).toFixed(3)}
                </div>
                <p className="text-xs text-muted-foreground">
                  Mesh connectivity
                </p>
              </CardContent>
            </Card>
          </div>

          <div className="grid grid-cols-1 lg:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>Mesh Nodes</CardTitle>
                <CardDescription>
                  Active agents in the discovery mesh
                </CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-3 max-h-96 overflow-y-auto">
                  {meshTopology?.nodes.map((node) => (
                    <div
                      key={node.id}
                      className={`p-3 rounded-lg border cursor-pointer transition-colors ${
                        selectedNode === node.id ? 'bg-blue-50 border-blue-200' : 'hover:bg-gray-50'
                      }`}
                      onClick={() => setSelectedNode(selectedNode === node.id ? null : node.id)}
                    >
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          <div className={`w-2 h-2 rounded-full ${getNodeStatusColor(node.status)}`} />
                          <span className="font-medium">{node.id}</span>
                          <Badge variant="outline" className="text-xs">
                            {node.type}
                          </Badge>
                        </div>
                        <div className="text-sm text-muted-foreground">
                          {node.resource_count} resources
                        </div>
                      </div>
                      
                      {selectedNode === node.id && (
                        <div className="mt-3 space-y-2 text-sm">
                          <div>
                            <span className="font-medium">Capabilities:</span>
                            <div className="flex flex-wrap gap-1 mt-1">
                              {node.capabilities.map((cap) => (
                                <Badge key={cap} variant="secondary" className="text-xs">
                                  {cap}
                                </Badge>
                              ))}
                            </div>
                          </div>
                          <div className="flex justify-between">
                            <span>Connections: {node.connection_count}</span>
                            <span>Last Activity: {new Date(node.last_activity * 1000).toLocaleTimeString()}</span>
                          </div>
                        </div>
                      )}
                    </div>
                  ))}
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Mesh Actions</CardTitle>
                <CardDescription>
                  Optimize and manage the discovery mesh
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <Button
                  onClick={optimizeMesh}
                  disabled={isLoading}
                  className="w-full"
                >
                  <Network className="h-4 w-4 mr-2" />
                  Optimize Mesh Topology
                </Button>
                
                <Button
                  variant="outline"
                  onClick={clearDiscoveryCache}
                  className="w-full"
                >
                  <RefreshCw className="h-4 w-4 mr-2" />
                  Clear Discovery Cache
                </Button>

                <div className="p-4 bg-gray-50 rounded-lg">
                  <h4 className="font-medium mb-2">Mesh Health</h4>
                  <div className="space-y-2 text-sm">
                    <div className="flex justify-between">
                      <span>Discovery Efficiency:</span>
                      <span className="font-medium">
                        {((meshTopology?.statistics.discovery_efficiency || 0) * 100).toFixed(1)}%
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span>Average Path Length:</span>
                      <span className="font-medium">
                        {meshTopology?.statistics.average_path_length?.toFixed(2) || 0}
                      </span>
                    </div>
                    <div className="flex justify-between">
                      <span>Cluster Coefficient:</span>
                      <span className="font-medium">
                        {(meshTopology?.statistics.cluster_coefficient || 0).toFixed(3)}
                      </span>
                    </div>
                  </div>
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="discovery" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Resource Discovery</CardTitle>
              <CardDescription>
                Search for resources across the agent fleet
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="flex gap-4">
                <div className="flex-1">
                  <Label htmlFor="discovery-query">Search Query</Label>
                  <Input
                    id="discovery-query"
                    placeholder="Enter search query (e.g., weather, calculation, memory)"
                    value={discoveryQuery}
                    onChange={(e) => setDiscoveryQuery(e.target.value)}
                    onKeyPress={(e) => e.key === 'Enter' && performDiscovery()}
                  />
                </div>
                <div>
                  <Label htmlFor="resource-type">Resource Type</Label>
                  <Select value={discoveryResourceType} onValueChange={setDiscoveryResourceType}>
                    <SelectTrigger className="w-32">
                      <SelectValue />
                    </SelectTrigger>
                    <SelectContent>
                      <SelectItem value="all">All</SelectItem>
                      <SelectItem value="tool">Tools</SelectItem>
                      <SelectItem value="memory">Memory</SelectItem>
                      <SelectItem value="file">Files</SelectItem>
                      <SelectItem value="conversation">Conversations</SelectItem>
                      <SelectItem value="message">Messages</SelectItem>
                    </SelectContent>
                  </Select>
                </div>
                <div className="flex items-end">
                  <Button onClick={performDiscovery} disabled={isLoading || !discoveryQuery.trim()}>
                    <Search className="h-4 w-4 mr-2" />
                    Discover
                  </Button>
                </div>
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader>
              <CardTitle>Discovery History</CardTitle>
              <CardDescription>
                Recent resource discovery operations
              </CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-4 max-h-96 overflow-y-auto">
                {discoveryHistory.map((discovery, index) => (
                  <div key={index} className="p-4 border rounded-lg">
                    <div className="flex items-center justify-between mb-2">
                      <div className="flex items-center gap-2">
                        <Badge className={getResourceTypeColor(discovery.resource_type)}>
                          {discovery.resource_type}
                        </Badge>
                        <span className="font-medium">"{discovery.query}"</span>
                      </div>
                      <div className="text-sm text-muted-foreground">
                        {discovery.discovery_time_ms}ms
                      </div>
                    </div>
                    
                    <div className="text-sm text-muted-foreground mb-2">
                      Found {discovery.total_results} results
                    </div>
                    
                    <div className="space-y-2">
                      {discovery.results.slice(0, 3).map((result, resultIndex) => (
                        <div key={resultIndex} className="flex items-center justify-between p-2 bg-gray-50 rounded">
                          <div className="flex items-center gap-2">
                            <Badge variant="outline" className="text-xs">
                              {result.resource_type}
                            </Badge>
                            <span className="font-medium">{result.name}</span>
                            <span className="text-xs text-muted-foreground">
                              by {result.owner_agent}
                            </span>
                          </div>
                          <div className="text-xs text-muted-foreground">
                            {result.discovery_time_ms}ms via {result.discovery_path.length} hops
                          </div>
                        </div>
                      ))}
                      {discovery.results.length > 3 && (
                        <div className="text-xs text-muted-foreground text-center">
                          +{discovery.results.length - 3} more results
                        </div>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="analytics" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            <Card>
              <CardHeader>
                <CardTitle>Discovery Performance</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  <div className="flex justify-between items-center">
                    <span>Average Discovery Time</span>
                    <span className="font-bold">
                      {discoveryHistory.length > 0
                        ? Math.round(discoveryHistory.reduce((sum, d) => sum + d.discovery_time_ms, 0) / discoveryHistory.length)
                        : 0}ms
                    </span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span>Success Rate</span>
                    <span className="font-bold">
                      {discoveryHistory.length > 0
                        ? Math.round((discoveryHistory.filter(d => d.total_results > 0).length / discoveryHistory.length) * 100)
                        : 0}%
                    </span>
                  </div>
                  <div className="flex justify-between items-center">
                    <span>Average Results</span>
                    <span className="font-bold">
                      {discoveryHistory.length > 0
                        ? Math.round(discoveryHistory.reduce((sum, d) => sum + d.total_results, 0) / discoveryHistory.length)
                        : 0}
                    </span>
                  </div>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader>
                <CardTitle>Resource Distribution</CardTitle>
              </CardHeader>
              <CardContent>
                <div className="space-y-4">
                  {['tool', 'memory', 'file', 'conversation', 'message'].map(type => {
                    const count = discoveryHistory.reduce((sum, d) => 
                      sum + d.results.filter(r => r.resource_type === type).length, 0
                    );
                    const percentage = discoveryHistory.length > 0 
                      ? (count / discoveryHistory.reduce((sum, d) => sum + d.results.length, 0)) * 100 
                      : 0;
                    
                    return (
                      <div key={type} className="space-y-2">
                        <div className="flex justify-between items-center">
                          <Badge className={getResourceTypeColor(type)}>{type}</Badge>
                          <span className="text-sm font-medium">{count} ({percentage.toFixed(1)}%)</span>
                        </div>
                        <Progress value={percentage} className="w-full" />
                      </div>
                    );
                  })}
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="settings" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>Discovery Configuration</CardTitle>
              <CardDescription>
                Configure discovery mesh behavior and optimization
              </CardDescription>
            </CardHeader>
            <CardContent className="space-y-4">
              <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
                <div>
                  <Label htmlFor="discovery-timeout">Discovery Timeout (ms)</Label>
                  <Input id="discovery-timeout" type="number" defaultValue="30000" />
                </div>
                <div>
                  <Label htmlFor="max-hops">Maximum Discovery Hops</Label>
                  <Input id="max-hops" type="number" defaultValue="5" />
                </div>
                <div>
                  <Label htmlFor="cache-ttl">Cache TTL (seconds)</Label>
                  <Input id="cache-ttl" type="number" defaultValue="300" />
                </div>
                <div>
                  <Label htmlFor="optimization-interval">Optimization Interval (minutes)</Label>
                  <Input id="optimization-interval" type="number" defaultValue="60" />
                </div>
              </div>
              
              <div className="flex gap-2">
                <Button variant="outline">Save Configuration</Button>
                <Button variant="outline">Reset to Defaults</Button>
              </div>
            </CardContent>
          </Card>
        </TabsContent>
      </Tabs>
    </div>
  );
}