import React, { useState, useEffect, useCallback } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Progress } from '@/components/ui/progress';
import { 
  AlertCircle, 
  CheckCircle, 
  XCircle, 
  Zap, 
  Activity,
  TrendingUp,
  Clock,
  Code,
  Wrench,
  RefreshCw,
  FileText,
  Search
} from 'lucide-react';
import { Input } from '@/components/ui/input';
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select';
import ErrorInterpretationPanel from './ErrorInterpretationPanel';

interface CrashReport {
  id: string;
  timestamp: string;
  process_name: string;
  pid: string;
  error_type: string;
  reason: string;
  location: {
    module: string;
    function: string;
    line: number;
  };
  stacktrace: Array<{
    module: string;
    function: string;
    arity: number;
    file: string;
    line: number;
  }>;
  severity: 'low' | 'medium' | 'high' | 'critical';
  frequency: number;
  last_occurrence: string;
  status: 'new' | 'analyzing' | 'analyzed' | 'fixing' | 'fixed';
}

interface CrashAnalysis {
  crash_id: string;
  root_cause: string;
  impact: string;
  related_issues: string[];
  confidence: number;
  suggested_actions: string[];
}

interface FixProposal {
  id: string;
  crash_id: string;
  description: string;
  code_changes: Array<{
    file: string;
    line: number;
    old_code: string;
    new_code: string;
    explanation: string;
  }>;
  risk_level: 'low' | 'medium' | 'high';
  estimated_effort: string;
  automated: boolean;
}

interface CrashStatistics {
  total_crashes: number;
  by_severity: Record<string, number>;
  by_status: Record<string, number>;
  by_module: Array<[string, number]>;
  recent_crashes: CrashReport[];
  crash_rate: number;
}

const CrashMonitorDashboard: React.FC = () => {
  const [crashes, setCrashes] = useState<CrashReport[]>([]);
  const [selectedCrash, setSelectedCrash] = useState<CrashReport | null>(null);
  const [analysis, setAnalysis] = useState<CrashAnalysis | null>(null);
  const [fixes, setFixes] = useState<FixProposal[]>([]);
  const [statistics, setStatistics] = useState<CrashStatistics | null>(null);
  const [loading, setLoading] = useState(true);
  const [searchTerm, setSearchTerm] = useState('');
  const [severityFilter, setSeverityFilter] = useState<string>('all');
  const [statusFilter, setStatusFilter] = useState<string>('all');
  const [ws, setWs] = useState<WebSocket | null>(null);

  // Fetch initial data
  useEffect(() => {
    fetchCrashes();
    fetchStatistics();
    connectWebSocket();

    return () => {
      if (ws) {
        ws.close();
      }
    };
  }, []);

  const connectWebSocket = () => {
    const websocket = new WebSocket(`ws://localhost:8080/ws/crashes`);
    
    websocket.onopen = () => {
      console.log('Connected to crash monitoring WebSocket');
    };

    websocket.onmessage = (event) => {
      const data = JSON.parse(event.data);
      handleWebSocketMessage(data);
    };

    websocket.onerror = (error) => {
      console.error('WebSocket error:', error);
    };

    websocket.onclose = () => {
      console.log('WebSocket connection closed');
      // Reconnect after 3 seconds
      setTimeout(connectWebSocket, 3000);
    };

    setWs(websocket);
  };

  const handleWebSocketMessage = (data: any) => {
    switch (data.type) {
      case 'new_crash':
        setCrashes(prev => [data.crash, ...prev]);
        fetchStatistics();
        break;
      case 'crash_update':
        setCrashes(prev => prev.map(c => 
          c.id === data.crash_id ? { ...c, ...data.updates } : c
        ));
        break;
      case 'analysis_complete':
        if (selectedCrash?.id === data.crash_id) {
          setAnalysis(data.analysis);
        }
        break;
      case 'fixes_ready':
        if (selectedCrash?.id === data.crash_id) {
          setFixes(data.fixes);
        }
        break;
    }
  };

  const fetchCrashes = async () => {
    try {
      const response = await fetch('/api/crashes');
      const data = await response.json();
      if (data.status === 'success') {
        setCrashes(data.reports);
      }
    } catch (error) {
      console.error('Failed to fetch crashes:', error);
    } finally {
      setLoading(false);
    }
  };

  const fetchStatistics = async () => {
    try {
      const response = await fetch('/api/crashes/stats');
      const data = await response.json();
      if (data.status === 'success') {
        setStatistics(data.statistics);
      }
    } catch (error) {
      console.error('Failed to fetch statistics:', error);
    }
  };

  const fetchAnalysis = async (crashId: string) => {
    try {
      const response = await fetch(`/api/crashes/${crashId}/analysis`);
      const data = await response.json();
      if (data.status === 'success') {
        setAnalysis(data.analysis);
      }
    } catch (error) {
      console.error('Failed to fetch analysis:', error);
    }
  };

  const fetchFixes = async (crashId: string) => {
    try {
      const response = await fetch(`/api/crashes/${crashId}/fixes`);
      const data = await response.json();
      if (data.status === 'success') {
        setFixes(data.fixes);
      }
    } catch (error) {
      console.error('Failed to fetch fixes:', error);
    }
  };

  const triggerAnalysis = async (crashId: string) => {
    try {
      const response = await fetch(`/api/crashes/${crashId}/analyze`, {
        method: 'POST'
      });
      const data = await response.json();
      if (data.status === 'accepted') {
        // Update crash status
        setCrashes(prev => prev.map(c => 
          c.id === crashId ? { ...c, status: 'analyzing' } : c
        ));
      }
    } catch (error) {
      console.error('Failed to trigger analysis:', error);
    }
  };

  const applyFix = async (crashId: string, fixId: string) => {
    try {
      const response = await fetch(`/api/crashes/${crashId}/fix`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ fix_id: fixId })
      });
      const data = await response.json();
      if (data.status === 'success') {
        // Update crash status
        setCrashes(prev => prev.map(c => 
          c.id === crashId ? { ...c, status: 'fixing' } : c
        ));
      }
    } catch (error) {
      console.error('Failed to apply fix:', error);
    }
  };

  const handleCrashSelect = (crash: CrashReport) => {
    setSelectedCrash(crash);
    setAnalysis(null);
    setFixes([]);
    fetchAnalysis(crash.id);
    fetchFixes(crash.id);
  };

  const getSeverityColor = (severity: string) => {
    switch (severity) {
      case 'critical': return 'bg-red-500';
      case 'high': return 'bg-orange-500';
      case 'medium': return 'bg-yellow-500';
      case 'low': return 'bg-blue-500';
      default: return 'bg-gray-500';
    }
  };

  const getStatusIcon = (status: string) => {
    switch (status) {
      case 'new': return <AlertCircle className="h-4 w-4" />;
      case 'analyzing': return <RefreshCw className="h-4 w-4 animate-spin" />;
      case 'analyzed': return <FileText className="h-4 w-4" />;
      case 'fixing': return <Wrench className="h-4 w-4" />;
      case 'fixed': return <CheckCircle className="h-4 w-4" />;
      default: return null;
    }
  };

  const filteredCrashes = crashes.filter(crash => {
    const matchesSearch = searchTerm === '' || 
      crash.location.module.toLowerCase().includes(searchTerm.toLowerCase()) ||
      crash.process_name.toLowerCase().includes(searchTerm.toLowerCase()) ||
      crash.error_type.toLowerCase().includes(searchTerm.toLowerCase());
    
    const matchesSeverity = severityFilter === 'all' || crash.severity === severityFilter;
    const matchesStatus = statusFilter === 'all' || crash.status === statusFilter;
    
    return matchesSearch && matchesSeverity && matchesStatus;
  });

  if (loading) {
    return (
      <div className="flex items-center justify-center h-screen">
        <RefreshCw className="h-8 w-8 animate-spin" />
      </div>
    );
  }

  return (
    <div className="container mx-auto p-4 space-y-4">
      {/* Header */}
      <div className="flex justify-between items-center">
        <h1 className="text-3xl font-bold">Crash Monitor Dashboard</h1>
        <Button onClick={() => { fetchCrashes(); fetchStatistics(); }}>
          <RefreshCw className="mr-2 h-4 w-4" />
          Refresh
        </Button>
      </div>

      {/* Statistics */}
      {statistics && (
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Total Crashes</CardTitle>
              <XCircle className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{statistics.total_crashes}</div>
              <p className="text-xs text-muted-foreground">
                {statistics.crash_rate} per hour
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Critical Issues</CardTitle>
              <AlertCircle className="h-4 w-4 text-red-500" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {statistics.by_severity.critical || 0}
              </div>
              <Progress 
                value={(statistics.by_severity.critical || 0) / statistics.total_crashes * 100} 
                className="mt-2"
              />
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Analyzed</CardTitle>
              <Activity className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {statistics.by_status.analyzed || 0}
              </div>
              <p className="text-xs text-muted-foreground">
                {statistics.by_status.analyzing || 0} in progress
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Fixed</CardTitle>
              <CheckCircle className="h-4 w-4 text-green-500" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {statistics.by_status.fixed || 0}
              </div>
              <p className="text-xs text-muted-foreground">
                {((statistics.by_status.fixed || 0) / statistics.total_crashes * 100).toFixed(1)}% resolution rate
              </p>
            </CardContent>
          </Card>
        </div>
      )}

      {/* Filters */}
      <div className="flex gap-4">
        <div className="flex-1">
          <Input
            placeholder="Search crashes..."
            value={searchTerm}
            onChange={(e) => setSearchTerm(e.target.value)}
            className="w-full"
          />
        </div>
        <Select value={severityFilter} onValueChange={setSeverityFilter}>
          <SelectTrigger className="w-[180px]">
            <SelectValue placeholder="Filter by severity" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Severities</SelectItem>
            <SelectItem value="critical">Critical</SelectItem>
            <SelectItem value="high">High</SelectItem>
            <SelectItem value="medium">Medium</SelectItem>
            <SelectItem value="low">Low</SelectItem>
          </SelectContent>
        </Select>
        <Select value={statusFilter} onValueChange={setStatusFilter}>
          <SelectTrigger className="w-[180px]">
            <SelectValue placeholder="Filter by status" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Statuses</SelectItem>
            <SelectItem value="new">New</SelectItem>
            <SelectItem value="analyzing">Analyzing</SelectItem>
            <SelectItem value="analyzed">Analyzed</SelectItem>
            <SelectItem value="fixing">Fixing</SelectItem>
            <SelectItem value="fixed">Fixed</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
        {/* Crash List */}
        <Card className="lg:col-span-1">
          <CardHeader>
            <CardTitle>Recent Crashes</CardTitle>
            <CardDescription>
              {filteredCrashes.length} crashes found
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ScrollArea className="h-[600px]">
              <div className="space-y-2">
                {filteredCrashes.map((crash) => (
                  <div
                    key={crash.id}
                    className={`p-3 border rounded-lg cursor-pointer hover:bg-gray-50 ${
                      selectedCrash?.id === crash.id ? 'border-blue-500 bg-blue-50' : ''
                    }`}
                    onClick={() => handleCrashSelect(crash)}
                  >
                    <div className="flex justify-between items-start">
                      <div className="flex-1">
                        <div className="flex items-center gap-2">
                          {getStatusIcon(crash.status)}
                          <span className="font-medium">{crash.location.module}</span>
                          <Badge className={getSeverityColor(crash.severity)}>
                            {crash.severity}
                          </Badge>
                        </div>
                        <p className="text-sm text-gray-600 mt-1">
                          {crash.location.function}:{crash.location.line}
                        </p>
                        <p className="text-xs text-gray-500 mt-1">
                          {crash.timestamp} • {crash.frequency}x
                        </p>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </ScrollArea>
          </CardContent>
        </Card>

        {/* Details Panel */}
        <Card className="lg:col-span-2">
          {selectedCrash ? (
            <Tabs defaultValue="details" className="h-full">
              <CardHeader>
                <div className="flex justify-between items-start">
                  <div>
                    <CardTitle>{selectedCrash.location.module}</CardTitle>
                    <CardDescription>
                      {selectedCrash.error_type} in {selectedCrash.location.function}
                    </CardDescription>
                  </div>
                  <div className="flex gap-2">
                    {selectedCrash.status === 'new' && (
                      <Button 
                        size="sm" 
                        onClick={() => triggerAnalysis(selectedCrash.id)}
                      >
                        <Zap className="mr-2 h-4 w-4" />
                        Analyze
                      </Button>
                    )}
                  </div>
                </div>
              </CardHeader>
              
              <TabsList className="grid w-full grid-cols-4">
                <TabsTrigger value="details">Details</TabsTrigger>
                <TabsTrigger value="stacktrace">Stacktrace</TabsTrigger>
                <TabsTrigger value="analysis">Analysis</TabsTrigger>
                <TabsTrigger value="fixes">Fixes</TabsTrigger>
              </TabsList>

              <CardContent>
                <TabsContent value="details" className="space-y-4">
                  <div>
                    <h3 className="font-semibold mb-2">Error Information</h3>
                    <div className="space-y-2">
                      <div className="flex justify-between">
                        <span className="text-gray-600">Process:</span>
                        <span className="font-mono">{selectedCrash.process_name}</span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-gray-600">PID:</span>
                        <span className="font-mono">{selectedCrash.pid}</span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-gray-600">Frequency:</span>
                        <span>{selectedCrash.frequency} occurrences</span>
                      </div>
                      <div className="flex justify-between">
                        <span className="text-gray-600">Last seen:</span>
                        <span>{selectedCrash.last_occurrence}</span>
                      </div>
                    </div>
                  </div>

                  <div>
                    <h3 className="font-semibold mb-2">Reason</h3>
                    <pre className="bg-gray-100 p-3 rounded overflow-x-auto text-sm">
                      {selectedCrash.reason}
                    </pre>
                  </div>
                </TabsContent>

                <TabsContent value="stacktrace" className="space-y-4">
                  <ScrollArea className="h-[400px]">
                    <div className="space-y-2">
                      {selectedCrash.stacktrace.map((frame, index) => (
                        <div key={index} className="border rounded p-3">
                          <div className="font-mono text-sm">
                            {frame.module}:{frame.function}/{frame.arity}
                          </div>
                          {frame.file && (
                            <div className="text-xs text-gray-600 mt-1">
                              {frame.file}:{frame.line}
                            </div>
                          )}
                        </div>
                      ))}
                    </div>
                  </ScrollArea>
                </TabsContent>

                <TabsContent value="analysis" className="space-y-4">
                  {analysis ? (
                    <>
                      <Alert>
                        <AlertCircle className="h-4 w-4" />
                        <AlertTitle>Root Cause</AlertTitle>
                        <AlertDescription>{analysis.root_cause}</AlertDescription>
                      </Alert>

                      <div>
                        <h3 className="font-semibold mb-2">Impact</h3>
                        <p className="text-gray-700">{analysis.impact}</p>
                      </div>

                      <div>
                        <h3 className="font-semibold mb-2">Confidence</h3>
                        <Progress value={analysis.confidence * 100} />
                        <p className="text-sm text-gray-600 mt-1">
                          {(analysis.confidence * 100).toFixed(0)}% confidence
                        </p>
                      </div>

                      {analysis.related_issues.length > 0 && (
                        <div>
                          <h3 className="font-semibold mb-2">Related Issues</h3>
                          <ul className="list-disc list-inside space-y-1">
                            {analysis.related_issues.map((issue, index) => (
                              <li key={index} className="text-gray-700">{issue}</li>
                            ))}
                          </ul>
                        </div>
                      )}

                      {analysis.suggested_actions.length > 0 && (
                        <div>
                          <h3 className="font-semibold mb-2">Suggested Actions</h3>
                          <ul className="list-disc list-inside space-y-1">
                            {analysis.suggested_actions.map((action, index) => (
                              <li key={index} className="text-gray-700">{action}</li>
                            ))}
                          </ul>
                        </div>
                      )}
                    </>
                  ) : (
                    <div className="text-center py-8">
                      <p className="text-gray-500">No analysis available</p>
                      <Button 
                        className="mt-4" 
                        onClick={() => triggerAnalysis(selectedCrash.id)}
                      >
                        <Zap className="mr-2 h-4 w-4" />
                        Run Analysis
                      </Button>
                    </div>
                  )}
                </TabsContent>

                <TabsContent value="fixes" className="space-y-4">
                  {fixes.length > 0 ? (
                    <div className="space-y-4">
                      {fixes.map((fix) => (
                        <Card key={fix.id}>
                          <CardHeader>
                            <div className="flex justify-between items-start">
                              <div>
                                <CardTitle className="text-lg">{fix.description}</CardTitle>
                                <div className="flex gap-2 mt-2">
                                  <Badge variant={
                                    fix.risk_level === 'low' ? 'default' : 
                                    fix.risk_level === 'medium' ? 'secondary' : 'destructive'
                                  }>
                                    {fix.risk_level} risk
                                  </Badge>
                                  <Badge variant="outline">
                                    {fix.estimated_effort}
                                  </Badge>
                                  {fix.automated && (
                                    <Badge variant="secondary">
                                      <Zap className="mr-1 h-3 w-3" />
                                      Automated
                                    </Badge>
                                  )}
                                </div>
                              </div>
                              <Button 
                                size="sm"
                                onClick={() => applyFix(selectedCrash.id, fix.id)}
                                disabled={!fix.automated}
                              >
                                Apply Fix
                              </Button>
                            </div>
                          </CardHeader>
                          <CardContent>
                            <div className="space-y-3">
                              {fix.code_changes.map((change, index) => (
                                <div key={index} className="border rounded p-3">
                                  <div className="flex justify-between items-center mb-2">
                                    <span className="font-mono text-sm">
                                      {change.file}:{change.line}
                                    </span>
                                  </div>
                                  <div className="grid grid-cols-2 gap-2">
                                    <div>
                                      <p className="text-xs text-gray-600 mb-1">Before:</p>
                                      <pre className="bg-red-50 p-2 rounded text-xs overflow-x-auto">
                                        {change.old_code}
                                      </pre>
                                    </div>
                                    <div>
                                      <p className="text-xs text-gray-600 mb-1">After:</p>
                                      <pre className="bg-green-50 p-2 rounded text-xs overflow-x-auto">
                                        {change.new_code}
                                      </pre>
                                    </div>
                                  </div>
                                  <p className="text-sm text-gray-600 mt-2">
                                    {change.explanation}
                                  </p>
                                </div>
                              ))}
                            </div>
                          </CardContent>
                        </Card>
                      ))}
                    </div>
                  ) : (
                    <div className="text-center py-8">
                      <p className="text-gray-500">No fixes available</p>
                    </div>
                  )}
                </TabsContent>
              </CardContent>
            </Tabs>
          ) : (
            <div className="flex items-center justify-center h-full">
              <p className="text-gray-500">Select a crash to view details</p>
            </div>
          )}
        </Card>
      </div>
    </div>
  );
};

export default CrashMonitorDashboard;