import React, { useState, useEffect } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Badge } from '@/components/ui/badge';
import { Alert, AlertDescription, AlertTitle } from '@/components/ui/alert';
import { Button } from '@/components/ui/button';
import { ScrollArea } from '@/components/ui/scroll-area';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Progress } from '@/components/ui/progress';
import { Textarea } from '@/components/ui/textarea';
import {
  AlertCircle,
  Info,
  Lightbulb,
  TrendingUp,
  Brain,
  Search,
  RefreshCw,
  CheckCircle,
  XCircle,
  AlertTriangle,
  Zap,
  MessageSquare,
  Tag,
  Clock
} from 'lucide-react';

interface ErrorInterpretation {
  id: string;
  timestamp: string;
  error_type: 'error' | 'warning' | 'info';
  original_text: string;
  interpretation: string;
  severity: 'low' | 'medium' | 'high' | 'critical';
  category: string;
  tags: string[];
  suggestions: string[];
  related_errors: string[];
  confidence: number;
  context: {
    root_cause?: string;
    common_scenarios?: string[];
    prevention?: string;
    action_required?: boolean;
    related_components?: string[];
  };
}

interface ErrorStats {
  total: number;
  by_severity: Record<string, number>;
  by_category: Record<string, number>;
  recent_trend: number;
}

const ErrorInterpretationPanel: React.FC = () => {
  const [interpretations, setInterpretations] = useState<ErrorInterpretation[]>([]);
  const [selectedInterpretation, setSelectedInterpretation] = useState<ErrorInterpretation | null>(null);
  const [stats, setStats] = useState<ErrorStats | null>(null);
  const [customError, setCustomError] = useState('');
  const [interpreting, setInterpreting] = useState(false);
  const [eventSource, setEventSource] = useState<EventSource | null>(null);

  useEffect(() => {
    fetchInterpretations();
    subscribeToInterpretations();

    return () => {
      if (eventSource) {
        eventSource.close();
      }
    };
  }, []);

  const fetchInterpretations = async () => {
    try {
      const response = await fetch('/api/interpretations');
      const data = await response.json();
      if (data.status === 'success') {
        setInterpretations(data.interpretations);
        calculateStats(data.interpretations);
      }
    } catch (error) {
      console.error('Failed to fetch interpretations:', error);
    }
  };

  const subscribeToInterpretations = () => {
    const source = new EventSource('/api/interpretations/subscribe');

    source.addEventListener('interpretation', (event) => {
      const interpretation = JSON.parse(event.data);
      handleNewInterpretation(interpretation);
    });

    source.addEventListener('error', (error) => {
      console.error('SSE error:', error);
      source.close();
      // Reconnect after 5 seconds
      setTimeout(subscribeToInterpretations, 5000);
    });

    setEventSource(source);
  };

  const handleNewInterpretation = (interpretation: ErrorInterpretation) => {
    setInterpretations(prev => [interpretation, ...prev.slice(0, 99)]);
    
    // Update stats
    setStats(prev => {
      if (!prev) return null;
      return {
        ...prev,
        total: prev.total + 1,
        by_severity: {
          ...prev.by_severity,
          [interpretation.severity]: (prev.by_severity[interpretation.severity] || 0) + 1
        },
        by_category: {
          ...prev.by_category,
          [interpretation.category]: (prev.by_category[interpretation.category] || 0) + 1
        }
      };
    });
  };

  const calculateStats = (interps: ErrorInterpretation[]) => {
    const stats: ErrorStats = {
      total: interps.length,
      by_severity: {},
      by_category: {},
      recent_trend: 0
    };

    interps.forEach(interp => {
      stats.by_severity[interp.severity] = (stats.by_severity[interp.severity] || 0) + 1;
      stats.by_category[interp.category] = (stats.by_category[interp.category] || 0) + 1;
    });

    // Calculate trend (errors in last hour vs previous hour)
    const oneHourAgo = Date.now() - 3600000;
    const twoHoursAgo = Date.now() - 7200000;
    const recentCount = interps.filter(i => new Date(i.timestamp).getTime() > oneHourAgo).length;
    const previousCount = interps.filter(i => {
      const time = new Date(i.timestamp).getTime();
      return time > twoHoursAgo && time <= oneHourAgo;
    }).length;
    
    stats.recent_trend = previousCount > 0 ? ((recentCount - previousCount) / previousCount) * 100 : 0;

    setStats(stats);
  };

  const interpretCustomError = async () => {
    if (!customError.trim()) return;

    setInterpreting(true);
    try {
      const response = await fetch('/api/interpretations/interpret', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ error_text: customError })
      });

      if (response.ok) {
        setCustomError('');
      }
    } catch (error) {
      console.error('Failed to interpret error:', error);
    } finally {
      setInterpreting(false);
    }
  };

  const getSeverityIcon = (severity: string) => {
    switch (severity) {
      case 'critical': return <XCircle className="h-4 w-4 text-red-500" />;
      case 'high': return <AlertCircle className="h-4 w-4 text-orange-500" />;
      case 'medium': return <AlertTriangle className="h-4 w-4 text-yellow-500" />;
      case 'low': return <Info className="h-4 w-4 text-blue-500" />;
      default: return null;
    }
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

  const getConfidenceColor = (confidence: number) => {
    if (confidence >= 0.8) return 'text-green-600';
    if (confidence >= 0.5) return 'text-yellow-600';
    return 'text-red-600';
  };

  return (
    <div className="space-y-4">
      {/* Header */}
      <div className="flex justify-between items-center">
        <div>
          <h2 className="text-2xl font-bold">AI Error Interpretation</h2>
          <p className="text-muted-foreground">
            Real-time AI analysis of system errors and logs
          </p>
        </div>
        <Button onClick={fetchInterpretations}>
          <RefreshCw className="mr-2 h-4 w-4" />
          Refresh
        </Button>
      </div>

      {/* Statistics */}
      {stats && (
        <div className="grid grid-cols-1 md:grid-cols-4 gap-4">
          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Total Errors</CardTitle>
              <MessageSquare className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{stats.total}</div>
              <div className="flex items-center text-xs text-muted-foreground">
                <TrendingUp className={`mr-1 h-3 w-3 ${stats.recent_trend > 0 ? 'text-red-500' : 'text-green-500'}`} />
                {Math.abs(stats.recent_trend).toFixed(1)}% vs last hour
              </div>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Critical Issues</CardTitle>
              <AlertCircle className="h-4 w-4 text-red-500" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{stats.by_severity.critical || 0}</div>
              <Progress 
                value={(stats.by_severity.critical || 0) / stats.total * 100} 
                className="mt-2"
              />
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">Categories</CardTitle>
              <Tag className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">{Object.keys(stats.by_category).length}</div>
              <p className="text-xs text-muted-foreground">
                Unique error types
              </p>
            </CardContent>
          </Card>

          <Card>
            <CardHeader className="flex flex-row items-center justify-between space-y-0 pb-2">
              <CardTitle className="text-sm font-medium">AI Confidence</CardTitle>
              <Brain className="h-4 w-4 text-muted-foreground" />
            </CardHeader>
            <CardContent>
              <div className="text-2xl font-bold">
                {interpretations.length > 0 
                  ? (interpretations.reduce((acc, i) => acc + i.confidence, 0) / interpretations.length * 100).toFixed(0)
                  : 0}%
              </div>
              <p className="text-xs text-muted-foreground">
                Average confidence
              </p>
            </CardContent>
          </Card>
        </div>
      )}

      {/* Custom Error Interpretation */}
      <Card>
        <CardHeader>
          <CardTitle>Interpret Custom Error</CardTitle>
          <CardDescription>
            Paste any error message or log line for AI interpretation
          </CardDescription>
        </CardHeader>
        <CardContent className="space-y-4">
          <Textarea
            placeholder="Paste error message or log line here..."
            value={customError}
            onChange={(e) => setCustomError(e.target.value)}
            rows={3}
          />
          <Button 
            onClick={interpretCustomError} 
            disabled={interpreting || !customError.trim()}
          >
            {interpreting ? (
              <>
                <RefreshCw className="mr-2 h-4 w-4 animate-spin" />
                Interpreting...
              </>
            ) : (
              <>
                <Brain className="mr-2 h-4 w-4" />
                Interpret Error
              </>
            )}
          </Button>
        </CardContent>
      </Card>

      {/* Main Content */}
      <div className="grid grid-cols-1 lg:grid-cols-3 gap-4">
        {/* Error List */}
        <Card className="lg:col-span-1">
          <CardHeader>
            <CardTitle>Recent Errors</CardTitle>
            <CardDescription>
              Click to view AI interpretation
            </CardDescription>
          </CardHeader>
          <CardContent>
            <ScrollArea className="h-[600px]">
              <div className="space-y-2">
                {interpretations.map((interp) => (
                  <div
                    key={interp.id}
                    className={`p-3 border rounded-lg cursor-pointer hover:bg-gray-50 ${
                      selectedInterpretation?.id === interp.id ? 'border-blue-500 bg-blue-50' : ''
                    }`}
                    onClick={() => setSelectedInterpretation(interp)}
                  >
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2">
                          {getSeverityIcon(interp.severity)}
                          <span className="font-medium text-sm">{interp.category}</span>
                          <Badge variant="outline" className="text-xs">
                            {interp.error_type}
                          </Badge>
                        </div>
                        <p className="text-xs text-gray-600 mt-1 line-clamp-2">
                          {interp.interpretation}
                        </p>
                        <div className="flex items-center gap-2 mt-1">
                          <Clock className="h-3 w-3 text-gray-400" />
                          <span className="text-xs text-gray-500">{interp.timestamp}</span>
                          <span className={`text-xs ${getConfidenceColor(interp.confidence)}`}>
                            {(interp.confidence * 100).toFixed(0)}% confidence
                          </span>
                        </div>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            </ScrollArea>
          </CardContent>
        </Card>

        {/* Interpretation Details */}
        <Card className="lg:col-span-2">
          {selectedInterpretation ? (
            <Tabs defaultValue="interpretation" className="h-full">
              <CardHeader>
                <div className="flex justify-between items-start">
                  <div>
                    <CardTitle className="flex items-center gap-2">
                      {getSeverityIcon(selectedInterpretation.severity)}
                      {selectedInterpretation.category}
                    </CardTitle>
                    <CardDescription>
                      AI Interpretation • {(selectedInterpretation.confidence * 100).toFixed(0)}% confidence
                    </CardDescription>
                  </div>
                  <div className="flex gap-2">
                    {selectedInterpretation.tags.map((tag) => (
                      <Badge key={tag} variant="secondary">
                        {tag}
                      </Badge>
                    ))}
                  </div>
                </div>
              </CardHeader>

              <TabsList className="grid w-full grid-cols-4">
                <TabsTrigger value="interpretation">Interpretation</TabsTrigger>
                <TabsTrigger value="original">Original</TabsTrigger>
                <TabsTrigger value="suggestions">Suggestions</TabsTrigger>
                <TabsTrigger value="context">Context</TabsTrigger>
              </TabsList>

              <CardContent>
                <TabsContent value="interpretation" className="space-y-4">
                  <Alert>
                    <Brain className="h-4 w-4" />
                    <AlertTitle>AI Interpretation</AlertTitle>
                    <AlertDescription className="mt-2">
                      {selectedInterpretation.interpretation}
                    </AlertDescription>
                  </Alert>

                  {selectedInterpretation.context.root_cause && (
                    <div>
                      <h3 className="font-semibold mb-2 flex items-center gap-2">
                        <Zap className="h-4 w-4" />
                        Root Cause
                      </h3>
                      <p className="text-gray-700">{selectedInterpretation.context.root_cause}</p>
                    </div>
                  )}

                  <div>
                    <h3 className="font-semibold mb-2">Severity Assessment</h3>
                    <div className="flex items-center gap-4">
                      <Badge className={getSeverityColor(selectedInterpretation.severity)}>
                        {selectedInterpretation.severity}
                      </Badge>
                      {selectedInterpretation.context.action_required && (
                        <Badge variant="destructive">Action Required</Badge>
                      )}
                    </div>
                  </div>
                </TabsContent>

                <TabsContent value="original" className="space-y-4">
                  <div>
                    <h3 className="font-semibold mb-2">Original Error</h3>
                    <pre className="bg-gray-100 p-4 rounded overflow-x-auto text-sm">
                      {selectedInterpretation.original_text}
                    </pre>
                  </div>
                </TabsContent>

                <TabsContent value="suggestions" className="space-y-4">
                  {selectedInterpretation.suggestions.length > 0 ? (
                    <div>
                      <h3 className="font-semibold mb-3 flex items-center gap-2">
                        <Lightbulb className="h-4 w-4" />
                        AI Suggestions
                      </h3>
                      <div className="space-y-3">
                        {selectedInterpretation.suggestions.map((suggestion, index) => (
                          <Alert key={index}>
                            <CheckCircle className="h-4 w-4" />
                            <AlertDescription>{suggestion}</AlertDescription>
                          </Alert>
                        ))}
                      </div>
                    </div>
                  ) : (
                    <p className="text-gray-500">No specific suggestions available</p>
                  )}

                  {selectedInterpretation.context.prevention && (
                    <div>
                      <h3 className="font-semibold mb-2">Prevention</h3>
                      <Alert>
                        <AlertCircle className="h-4 w-4" />
                        <AlertDescription>
                          {selectedInterpretation.context.prevention}
                        </AlertDescription>
                      </Alert>
                    </div>
                  )}
                </TabsContent>

                <TabsContent value="context" className="space-y-4">
                  {selectedInterpretation.context.common_scenarios && 
                   selectedInterpretation.context.common_scenarios.length > 0 && (
                    <div>
                      <h3 className="font-semibold mb-2">Common Scenarios</h3>
                      <ul className="list-disc list-inside space-y-1">
                        {selectedInterpretation.context.common_scenarios.map((scenario, index) => (
                          <li key={index} className="text-gray-700">{scenario}</li>
                        ))}
                      </ul>
                    </div>
                  )}

                  {selectedInterpretation.context.related_components && 
                   selectedInterpretation.context.related_components.length > 0 && (
                    <div>
                      <h3 className="font-semibold mb-2">Related Components</h3>
                      <div className="flex flex-wrap gap-2">
                        {selectedInterpretation.context.related_components.map((component, index) => (
                          <Badge key={index} variant="outline">{component}</Badge>
                        ))}
                      </div>
                    </div>
                  )}

                  {selectedInterpretation.related_errors.length > 0 && (
                    <div>
                      <h3 className="font-semibold mb-2">Related Errors</h3>
                      <div className="space-y-2">
                        {selectedInterpretation.related_errors.map((error, index) => (
                          <div key={index} className="p-2 border rounded text-sm">
                            {error}
                          </div>
                        ))}
                      </div>
                    </div>
                  )}
                </TabsContent>
              </CardContent>
            </Tabs>
          ) : (
            <div className="flex items-center justify-center h-full">
              <div className="text-center">
                <Brain className="h-12 w-12 text-gray-400 mx-auto mb-4" />
                <p className="text-gray-500">Select an error to view AI interpretation</p>
              </div>
            </div>
          )}
        </Card>
      </div>
    </div>
  );
};

export default ErrorInterpretationPanel;