import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from "@/components/ui/card";
import { Alert, AlertDescription, AlertTitle } from "@/components/ui/alert";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { Progress } from "@/components/ui/progress";
import { 
  AlertCircle, 
  CheckCircle, 
  XCircle, 
  RefreshCw, 
  Activity,
  Server,
  Cpu,
  HardDrive,
  Zap,
  AlertTriangle,
  TrendingUp,
  TrendingDown,
  Clock
} from 'lucide-react';

interface ErrorEntry {
  id: string;
  timestamp: string;
  location: {
    module: string;
    function: string;
    line: number;
  };
  error_type: string;
  message: string;
  severity: 'critical' | 'error' | 'warning' | 'info';
  count: number;
  stacktrace?: any[];
}

interface SystemHealth {
  memory_ok: boolean;
  gc_health_ok: boolean;
  process_count_ok: boolean;
  scheduler_health_ok: boolean;
  message_queue_ok: boolean;
  memory_usage: number;
  process_count: number;
  scheduler_utilization: number;
}

interface ErrorSummary {
  total_errors: number;
  error_types: Record<string, number>;
  severity_breakdown: Record<string, number>;
  recent_errors: ErrorEntry[];
}

interface MCPServer {
  id: string;
  name: string;
  status: 'connected' | 'error' | 'connecting' | 'disconnected';
  error?: string;
}

export function SystemMonitoringDashboard() {
  const [systemHealth, setSystemHealth] = useState<SystemHealth | null>(null);
  const [errorSummary, setErrorSummary] = useState<ErrorSummary | null>(null);
  const [errors, setErrors] = useState<ErrorEntry[]>([]);
  const [mcpServers, setMcpServers] = useState<MCPServer[]>([]);
  const [isLoading, setIsLoading] = useState(true);
  const [autoRefresh, setAutoRefresh] = useState(true);
  const [errorWs, setErrorWs] = useState<WebSocket | null>(null);

  useEffect(() => {
    fetchInitialData();
    connectErrorWebSocket();
    
    const interval = autoRefresh ? setInterval(fetchSystemData, 5000) : null;
    
    return () => {
      if (interval) clearInterval(interval);
      if (errorWs) errorWs.close();
    };
  }, [autoRefresh]);

  const connectErrorWebSocket = () => {
    const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
    const ws = new WebSocket(`${protocol}//${window.location.host}/ws/errors`);
    
    ws.onmessage = (event) => {
      const data = JSON.parse(event.data);
      if (data.type === 'error_logged') {
        setErrors(prev => [data.error, ...prev.slice(0, 99)]);
        updateErrorSummary(data.error);
      }
    };
    
    ws.onerror = (error) => {
      console.error('Error WebSocket error:', error);
    };
    
    setErrorWs(ws);
  };

  const fetchInitialData = async () => {
    setIsLoading(true);
    await Promise.all([
      fetchSystemData(),
      fetchErrors(),
      fetchMCPServers()
    ]);
    setIsLoading(false);
  };

  const fetchSystemData = async () => {
    try {
      const [healthRes, summaryRes] = await Promise.all([
        fetch('/api/system/health'),
        fetch('/api/errors/summary')
      ]);
      
      if (healthRes.ok) {
        const health = await healthRes.json();
        setSystemHealth(health);
      }
      
      if (summaryRes.ok) {
        const summary = await summaryRes.json();
        setErrorSummary(summary);
      }
    } catch (error) {
      console.error('Failed to fetch system data:', error);
    }
  };

  const fetchErrors = async () => {
    try {
      const response = await fetch('/api/errors?limit=100');
      if (response.ok) {
        const data = await response.json();
        setErrors(data);
      }
    } catch (error) {
      console.error('Failed to fetch errors:', error);
    }
  };

  const fetchMCPServers = async () => {
    try {
      const response = await fetch('/api/mcp/servers');
      if (response.ok) {
        const data = await response.json();
        setMcpServers(data.servers || []);
      }
    } catch (error) {
      console.error('Failed to fetch MCP servers:', error);
    }
  };

  const updateErrorSummary = (newError: ErrorEntry) => {
    setErrorSummary(prev => {
      if (!prev) return null;
      
      const errorType = newError.error_type;
      const severity = newError.severity;
      
      return {
        ...prev,
        total_errors: prev.total_errors + 1,
        error_types: {
          ...prev.error_types,
          [errorType]: (prev.error_types[errorType] || 0) + 1
        },
        severity_breakdown: {
          ...prev.severity_breakdown,
          [severity]: (prev.severity_breakdown[severity] || 0) + 1
        },
        recent_errors: [newError, ...prev.recent_errors.slice(0, 4)]
      };
    });
  };

  const clearErrors = async () => {
    try {
      const response = await fetch('/api/errors/clear', { method: 'POST' });
      if (response.ok) {
        setErrors([]);
        fetchSystemData();
      }
    } catch (error) {
      console.error('Failed to clear errors:', error);
    }
  };

  const getHealthStatus = () => {
    if (!systemHealth) return 'unknown';
    const checks = [
      systemHealth.memory_ok,
      systemHealth.gc_health_ok,
      systemHealth.process_count_ok,
      systemHealth.scheduler_health_ok,
      systemHealth.message_queue_ok
    ];
    const failedChecks = checks.filter(check => !check).length;
    
    if (failedChecks === 0) return 'healthy';
    if (failedChecks <= 2) return 'warning';
    return 'critical';
  };

  const getSeverityColor = (severity: string) => {
    switch (severity) {
      case 'critical': return 'text-red-600 bg-red-100';
      case 'error': return 'text-orange-600 bg-orange-100';
      case 'warning': return 'text-yellow-600 bg-yellow-100';
      case 'info': return 'text-blue-600 bg-blue-100';
      default: return 'text-gray-600 bg-gray-100';
    }
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'healthy':
      case 'connected':
        return <CheckCircle className="h-5 w-5 text-green-500" />;
      case 'warning':
      case 'connecting':
        return <AlertTriangle className="h-5 w-5 text-yellow-500" />;
      case 'critical':
      case 'error':
      case 'disconnected':
        return <XCircle className="h-5 w-5 text-red-500" />;
      default:
        return <AlertCircle className="h-5 w-5 text-gray-500" />;
    }
  };

  if (isLoading) {
    return (
      <div className="flex items-center justify-center h-full">
        <RefreshCw className="h-8 w-8 animate-spin text-blue-500" />
      </div>
    );
  }

  const healthStatus = getHealthStatus();

  return (
    <div className="p-6 space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <h1 className="text-3xl font-bold">System Monitoring Dashboard</h1>
        <div className="flex gap-2">
          <Button
            variant={autoRefresh ? "default" : "outline"}
            onClick={() => setAutoRefresh(!autoRefresh)}
          >
            <RefreshCw className={`h-4 w-4 mr-2 ${autoRefresh ? 'animate-spin' : ''}`} />
            {autoRefresh ? 'Auto-refresh ON' : 'Auto-refresh OFF'}
          </Button>
          <Button variant="outline" onClick={fetchInitialData}>
            <RefreshCw className="h-4 w-4 mr-2" />
            Refresh Now
          </Button>
        </div>
      </div>

      {/* System Health Alert */}
      {healthStatus !== 'healthy' && (
        <Alert variant={healthStatus === 'critical' ? 'destructive' : 'default'}>
          <AlertCircle className="h-4 w-4" />
          <AlertTitle>System Health {healthStatus === 'critical' ? 'Critical' : 'Warning'}</AlertTitle>
          <AlertDescription>
            {systemHealth && (
              <div className="mt-2 space-y-1">
                {!systemHealth.memory_ok && <div>• High memory usage detected</div>}
                {!systemHealth.gc_health_ok && <div>• Garbage collection issues detected</div>}
                {!systemHealth.process_count_ok && <div>• High process count detected</div>}
                {!systemHealth.scheduler_health_ok && <div>• Scheduler performance issues</div>}
                {!systemHealth.message_queue_ok && <div>• Message queue backlog detected</div>}
              </div>
            )}
          </AlertDescription>
        </Alert>
      )}

      <Tabs defaultValue="overview" className="space-y-4">
        <TabsList className="grid w-full grid-cols-4">
          <TabsTrigger value="overview">Overview</TabsTrigger>
          <TabsTrigger value="errors">Errors</TabsTrigger>
          <TabsTrigger value="mcp">MCP Servers</TabsTrigger>
          <TabsTrigger value="metrics">Metrics</TabsTrigger>
        </TabsList>

        {/* Overview Tab */}
        <TabsContent value="overview" className="space-y-4">
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            {/* System Health Card */}
            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">System Health</CardTitle>
                {getStatusIcon(healthStatus)}
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold capitalize">{healthStatus}</div>
                <p className="text-xs text-muted-foreground mt-1">
                  {systemHealth && `${Object.values(systemHealth).filter(v => v === true).length}/5 checks passing`}
                </p>
              </CardContent>
            </Card>

            {/* Total Errors Card */}
            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">Total Errors</CardTitle>
                <Zap className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">{errorSummary?.total_errors || 0}</div>
                <p className="text-xs text-muted-foreground mt-1">
                  Across all services
                </p>
              </CardContent>
            </Card>

            {/* MCP Servers Status */}
            <Card>
              <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
                <CardTitle className="text-sm font-medium">MCP Servers</CardTitle>
                <Server className="h-4 w-4 text-muted-foreground" />
              </CardHeader>
              <CardContent>
                <div className="text-2xl font-bold">
                  {mcpServers.filter(s => s.status === 'connected').length}/{mcpServers.length}
                </div>
                <p className="text-xs text-muted-foreground mt-1">
                  Connected servers
                </p>
              </CardContent>
            </Card>
          </div>

          {/* Recent Errors */}
          {errorSummary && errorSummary.recent_errors.length > 0 && (
            <Card>
              <CardHeader>
                <CardTitle>Recent Errors</CardTitle>
                <CardDescription>Latest errors detected in the system</CardDescription>
              </CardHeader>
              <CardContent>
                <div className="space-y-2">
                  {errorSummary.recent_errors.map((error) => (
                    <Alert key={error.id} className="py-2">
                      <div className="flex items-start justify-between">
                        <div className="flex-1">
                          <div className="flex items-center gap-2">
                            <Badge className={getSeverityColor(error.severity)}>
                              {error.severity}
                            </Badge>
                            <span className="text-sm font-medium">
                              {error.location.module}::{error.location.function}
                            </span>
                            <span className="text-xs text-muted-foreground">
                              Line {error.location.line}
                            </span>
                          </div>
                          <p className="text-sm mt-1">{error.message}</p>
                        </div>
                        <span className="text-xs text-muted-foreground">
                          {new Date(error.timestamp).toLocaleTimeString()}
                        </span>
                      </div>
                    </Alert>
                  ))}
                </div>
              </CardContent>
            </Card>
          )}
        </TabsContent>

        {/* Errors Tab */}
        <TabsContent value="errors" className="space-y-4">
          <Card>
            <CardHeader>
              <div className="flex justify-between items-center">
                <div>
                  <CardTitle>Error Log</CardTitle>
                  <CardDescription>Complete error history</CardDescription>
                </div>
                <Button variant="destructive" size="sm" onClick={clearErrors}>
                  Clear All Errors
                </Button>
              </div>
            </CardHeader>
            <CardContent>
              <ScrollArea className="h-[600px]">
                <div className="space-y-2">
                  {errors.map((error) => (
                    <div key={error.id} className="border rounded-lg p-3 space-y-2">
                      <div className="flex items-center justify-between">
                        <div className="flex items-center gap-2">
                          <Badge className={getSeverityColor(error.severity)}>
                            {error.severity}
                          </Badge>
                          <span className="font-mono text-sm">
                            {error.location.module}::{error.location.function}
                          </span>
                          {error.count > 1 && (
                            <Badge variant="secondary">×{error.count}</Badge>
                          )}
                        </div>
                        <span className="text-xs text-muted-foreground">
                          {new Date(error.timestamp).toLocaleString()}
                        </span>
                      </div>
                      <p className="text-sm">{error.message}</p>
                      {error.stacktrace && error.stacktrace.length > 0 && (
                        <details className="text-xs">
                          <summary className="cursor-pointer text-muted-foreground">
                            Stack trace
                          </summary>
                          <pre className="mt-2 bg-gray-100 p-2 rounded overflow-x-auto">
                            {JSON.stringify(error.stacktrace, null, 2)}
                          </pre>
                        </details>
                      )}
                    </div>
                  ))}
                </div>
              </ScrollArea>
            </CardContent>
          </Card>
        </TabsContent>

        {/* MCP Servers Tab */}
        <TabsContent value="mcp" className="space-y-4">
          <Card>
            <CardHeader>
              <CardTitle>MCP Server Status</CardTitle>
              <CardDescription>Model Context Protocol server connections</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                {mcpServers.map((server) => (
                  <div key={server.id} className="flex items-center justify-between p-3 border rounded-lg">
                    <div className="flex items-center gap-3">
                      {getStatusIcon(server.status)}
                      <div>
                        <p className="font-medium">{server.name}</p>
                        <p className="text-sm text-muted-foreground">ID: {server.id}</p>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <Badge variant={server.status === 'connected' ? 'default' : 'secondary'}>
                        {server.status}
                      </Badge>
                      {server.error && (
                        <span className="text-sm text-red-600">{server.error}</span>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        {/* Metrics Tab */}
        <TabsContent value="metrics" className="space-y-4">
          {systemHealth && (
            <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
              <Card>
                <CardHeader>
                  <CardTitle>Resource Usage</CardTitle>
                </CardHeader>
                <CardContent className="space-y-4">
                  <div>
                    <div className="flex justify-between mb-2">
                      <span className="text-sm font-medium">Memory Usage</span>
                      <span className="text-sm text-muted-foreground">
                        {((systemHealth.memory_usage || 0) * 100).toFixed(1)}%
                      </span>
                    </div>
                    <Progress value={(systemHealth.memory_usage || 0) * 100} />
                  </div>
                  <div>
                    <div className="flex justify-between mb-2">
                      <span className="text-sm font-medium">Scheduler Utilization</span>
                      <span className="text-sm text-muted-foreground">
                        {((systemHealth.scheduler_utilization || 0) * 100).toFixed(1)}%
                      </span>
                    </div>
                    <Progress value={(systemHealth.scheduler_utilization || 0) * 100} />
                  </div>
                  <div>
                    <div className="flex justify-between">
                      <span className="text-sm font-medium">Process Count</span>
                      <span className="text-sm text-muted-foreground">
                        {systemHealth.process_count || 0}
                      </span>
                    </div>
                  </div>
                </CardContent>
              </Card>

              {errorSummary && (
                <Card>
                  <CardHeader>
                    <CardTitle>Error Distribution</CardTitle>
                  </CardHeader>
                  <CardContent>
                    <div className="space-y-3">
                      <div>
                        <p className="text-sm font-medium mb-2">By Type</p>
                        {Object.entries(errorSummary.error_types).map(([type, count]) => (
                          <div key={type} className="flex justify-between py-1">
                            <span className="text-sm">{type}</span>
                            <Badge variant="secondary">{count}</Badge>
                          </div>
                        ))}
                      </div>
                      <div>
                        <p className="text-sm font-medium mb-2">By Severity</p>
                        {Object.entries(errorSummary.severity_breakdown).map(([severity, count]) => (
                          <div key={severity} className="flex justify-between py-1">
                            <Badge className={getSeverityColor(severity)}>{severity}</Badge>
                            <span className="text-sm">{count}</span>
                          </div>
                        ))}
                      </div>
                    </div>
                  </CardContent>
                </Card>
              )}
            </div>
          )}
        </TabsContent>
      </Tabs>
    </div>
  );
}