import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Dialog, DialogContent, DialogDescription, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Alert, AlertDescription } from '@/components/ui/alert';
import { 
  Search, 
  ExternalLink, 
  CheckCircle, 
  XCircle, 
  RefreshCw, 
  Settings,
  Zap,
  Globe,
  Activity
} from 'lucide-react';

interface PipedreamApp {
  name_slug: string;
  name: string;
  description?: string;
  category?: string;
  logo_url?: string;
}

interface PipedreamTool {
  name: string;
  description: string;
  app_slug: string;
  parameters: any;
  source: string;
}

interface ConnectionStatus {
  app_slug: string;
  status: 'connected' | 'not_connected' | 'error';
  connected_at?: number;
  last_verified?: number;
}

interface PipedreamStats {
  auth: {
    total_connections: number;
    active_users: number;
    auth_attempts: number;
    successful_auths: number;
  };
  discovery: {
    total_apps: number;
    total_tools: number;
    last_update: number;
    discovery_errors: number;
  };
}

export const PipedreamConnectionManager: React.FC = () => {
  const [apps, setApps] = useState<PipedreamApp[]>([]);
  const [userConnections, setUserConnections] = useState<Record<string, ConnectionStatus>>({});
  const [userTools, setUserTools] = useState<PipedreamTool[]>([]);
  const [stats, setStats] = useState<PipedreamStats | null>(null);
  const [searchTerm, setSearchTerm] = useState('');
  const [loading, setLoading] = useState(false);
  const [selectedApp, setSelectedApp] = useState<PipedreamApp | null>(null);
  const [appTools, setAppTools] = useState<PipedreamTool[]>([]);
  const [activeTab, setActiveTab] = useState('discover');

  const userId = 'demo_user'; // In a real app, this would come from authentication

  useEffect(() => {
    loadApps();
    loadUserConnections();
    loadUserTools();
    loadStats();
  }, []);

  const loadApps = async () => {
    try {
      const response = await fetch('/api/pipedream/apps');
      if (response.ok) {
        const result = await response.json();
        setApps(result.data || []);
      }
    } catch (error) {
      console.error('Failed to load apps:', error);
    }
  };

  const loadUserConnections = async () => {
    try {
      const response = await fetch('/api/pipedream/connections', {
        headers: { 'X-User-Id': userId }
      });
      if (response.ok) {
        const result = await response.json();
        setUserConnections(result.data || {});
      }
    } catch (error) {
      console.error('Failed to load user connections:', error);
    }
  };

  const loadUserTools = async () => {
    try {
      const response = await fetch('/api/pipedream/tools', {
        headers: { 'X-User-Id': userId }
      });
      if (response.ok) {
        const result = await response.json();
        setUserTools(result.data || []);
      }
    } catch (error) {
      console.error('Failed to load user tools:', error);
    }
  };

  const loadStats = async () => {
    try {
      const response = await fetch('/api/pipedream/stats');
      if (response.ok) {
        const result = await response.json();
        setStats(result.data);
      }
    } catch (error) {
      console.error('Failed to load stats:', error);
    }
  };

  const connectApp = async (appSlug: string) => {
    setLoading(true);
    try {
      const response = await fetch('/api/pipedream/connect', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-User-Id': userId
        },
        body: JSON.stringify({ app_slug: appSlug })
      });

      if (response.ok) {
        const result = await response.json();
        // Open connection URL in new window
        window.open(result.data.connection_url, '_blank', 'width=600,height=700');
      }
    } catch (error) {
      console.error('Failed to connect app:', error);
    } finally {
      setLoading(false);
    }
  };

  const disconnectApp = async (appSlug: string) => {
    setLoading(true);
    try {
      const response = await fetch('/api/pipedream/disconnect', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-User-Id': userId
        },
        body: JSON.stringify({ app_slug: appSlug })
      });

      if (response.ok) {
        await loadUserConnections();
        await loadUserTools();
      }
    } catch (error) {
      console.error('Failed to disconnect app:', error);
    } finally {
      setLoading(false);
    }
  };

  const refreshConnections = async () => {
    setLoading(true);
    try {
      const response = await fetch('/api/pipedream/refresh', {
        method: 'POST',
        headers: { 'X-User-Id': userId }
      });

      if (response.ok) {
        await loadUserConnections();
        await loadUserTools();
      }
    } catch (error) {
      console.error('Failed to refresh connections:', error);
    } finally {
      setLoading(false);
    }
  };

  const loadAppTools = async (app: PipedreamApp) => {
    try {
      const response = await fetch(`/api/pipedream/apps/${app.name_slug}/tools`);
      if (response.ok) {
        const result = await response.json();
        setAppTools(result.data || []);
        setSelectedApp(app);
      }
    } catch (error) {
      console.error('Failed to load app tools:', error);
    }
  };

  const filteredApps = apps.filter(app =>
    app.name.toLowerCase().includes(searchTerm.toLowerCase()) ||
    app.name_slug.toLowerCase().includes(searchTerm.toLowerCase()) ||
    (app.description && app.description.toLowerCase().includes(searchTerm.toLowerCase()))
  );

  const getConnectionStatus = (appSlug: string): 'connected' | 'not_connected' | 'error' => {
    return userConnections[appSlug]?.status || 'not_connected';
  };

  const formatTime = (timestamp: number): string => {
    return new Date(timestamp * 1000).toLocaleString();
  };

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex items-center justify-between">
        <div>
          <h2 className="text-2xl font-bold tracking-tight">Pipedream Integrations</h2>
          <p className="text-muted-foreground">
            Connect to 2,700+ apps and services through Pipedream MCP
          </p>
        </div>
        <Button onClick={refreshConnections} disabled={loading} variant="outline">
          <RefreshCw className={`h-4 w-4 mr-2 ${loading ? 'animate-spin' : ''}`} />
          Refresh
        </Button>
      </div>

      {/* Stats Cards */}
      {stats && (
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Available Apps</CardTitle>
              <Globe className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{stats.discovery.total_apps}</div>
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Available Tools</CardTitle>
              <Zap className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{stats.discovery.total_tools}</div>
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Your Connections</CardTitle>
              <Activity className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {Object.values(userConnections).filter(c => c.status === 'connected').length}
              </div>
            </CardContent>
          </Card>
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Your Tools</CardTitle>
              <Settings className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{userTools.length}</div>
            </CardContent>
          </Card>
        </div>
      )}

      {/* Main Content */}
      <Tabs value={activeTab} onValueChange={setActiveTab}>
        <TabsList className="grid w-full grid-cols-3">
          <TabsTrigger value="discover">Discover Apps</TabsTrigger>
          <TabsTrigger value="connected">My Connections</TabsTrigger>
          <TabsTrigger value="tools">My Tools</TabsTrigger>
        </TabsList>

        <TabsContent value="discover" className="space-y-4">
          <div className="flex items-center space-x-2">
            <Search className="h-4 w-4 text-muted-foreground" />
            <Input
              placeholder="Search apps..."
              value={searchTerm}
              onChange={(e) => setSearchTerm(e.target.value)}
              className="max-w-sm"
            />
          </div>

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
            {filteredApps.map((app) => (
              <Card key={app.name_slug} className="cursor-pointer hover:shadow-md transition-shadow">
                <CardHeader>
                  <div className="flex items-center justify-between">
                    <CardTitle className="text-lg">{app.name}</CardTitle>
                    <Badge 
                      variant={getConnectionStatus(app.name_slug) === 'connected' ? 'default' : 'secondary'}
                    >
                      {getConnectionStatus(app.name_slug) === 'connected' ? (
                        <CheckCircle className="h-3 w-3 mr-1" />
                      ) : (
                        <XCircle className="h-3 w-3 mr-1" />
                      )}
                      {getConnectionStatus(app.name_slug)}
                    </Badge>
                  </div>
                  <CardDescription className="line-clamp-2">
                    {app.description || `Integrate with ${app.name} through Pipedream`}
                  </CardDescription>
                </CardHeader>
                <CardContent>
                  <div className="flex space-x-2">
                    {getConnectionStatus(app.name_slug) === 'connected' ? (
                      <Button 
                        variant="destructive" 
                        size="sm" 
                        onClick={() => disconnectApp(app.name_slug)}
                        disabled={loading}
                      >
                        Disconnect
                      </Button>
                    ) : (
                      <Button 
                        size="sm" 
                        onClick={() => connectApp(app.name_slug)}
                        disabled={loading}
                      >
                        <ExternalLink className="h-3 w-3 mr-1" />
                        Connect
                      </Button>
                    )}
                    <Dialog>
                      <DialogTrigger asChild>
                        <Button 
                          variant="outline" 
                          size="sm"
                          onClick={() => loadAppTools(app)}
                        >
                          View Tools
                        </Button>
                      </DialogTrigger>
                      <DialogContent className="max-w-2xl max-h-[80vh]">
                        <DialogHeader>
                          <DialogTitle>{selectedApp?.name} Tools</DialogTitle>
                          <DialogDescription>
                            Available tools for {selectedApp?.name}
                          </DialogDescription>
                        </DialogHeader>
                        <ScrollArea className="h-96">
                          <div className="space-y-2">
                            {appTools.map((tool, index) => (
                              <Card key={index}>
                                <CardHeader className="pb-2">
                                  <CardTitle className="text-sm">{tool.name}</CardTitle>
                                  <CardDescription className="text-xs">
                                    {tool.description}
                                  </CardDescription>
                                </CardHeader>
                              </Card>
                            ))}
                          </div>
                        </ScrollArea>
                      </DialogContent>
                    </Dialog>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        <TabsContent value="connected" className="space-y-4">
          {Object.keys(userConnections).length === 0 ? (
            <Alert>
              <AlertDescription>
                No connected apps yet. Go to the Discover tab to connect your first app.
              </AlertDescription>
            </Alert>
          ) : (
            <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
              {Object.entries(userConnections).map(([appSlug, connection]) => (
                <Card key={appSlug}>
                  <CardHeader>
                    <div className="flex items-center justify-between">
                      <CardTitle className="text-lg capitalize">
                        {appSlug.replace(/_/g, ' ')}
                      </CardTitle>
                      <Badge variant={connection.status === 'connected' ? 'default' : 'destructive'}>
                        {connection.status}
                      </Badge>
                    </div>
                    {connection.connected_at && (
                      <CardDescription>
                        Connected: {formatTime(connection.connected_at)}
                      </CardDescription>
                    )}
                  </CardHeader>
                  <CardContent>
                    <Button 
                      variant="destructive" 
                      size="sm"
                      onClick={() => disconnectApp(appSlug)}
                      disabled={loading}
                    >
                      Disconnect
                    </Button>
                  </CardContent>
                </Card>
              ))}
            </div>
          )}
        </TabsContent>

        <TabsContent value="tools" className="space-y-4">
          {userTools.length === 0 ? (
            <Alert>
              <AlertDescription>
                No tools available. Connect some apps first to access their tools.
              </AlertDescription>
            </Alert>
          ) : (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              {userTools.map((tool, index) => (
                <Card key={index}>
                  <CardHeader>
                    <CardTitle className="text-lg">{tool.name}</CardTitle>
                    <CardDescription>{tool.description}</CardDescription>
                  </CardHeader>
                  <CardContent>
                    <div className="flex items-center justify-between">
                      <Badge variant="outline">{tool.app_slug}</Badge>
                      <Badge variant="secondary">Pipedream</Badge>
                    </div>
                  </CardContent>
                </Card>
              ))}
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
};

export default PipedreamConnectionManager;