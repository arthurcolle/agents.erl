import React, { useState, useEffect, useMemo, useCallback } from 'react';
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card';
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs';
import { Button } from '@/components/ui/button';
import { Badge } from '@/components/ui/badge';
import { Input } from '@/components/ui/input';
import { Label } from '@/components/ui/label';
import { Progress } from '@/components/ui/progress';
import { Switch } from '@/components/ui/switch';
import { Textarea } from '@/components/ui/textarea';
import { Slider } from '@/components/ui/slider';
import { 
  Activity,
  AlertTriangle,
  ArrowRight,
  BarChart3,
  Bot,
  Brain,
  ChevronDown,
  ChevronUp,
  CircuitBoard,
  Cpu,
  Database,
  GitBranch,
  Globe,
  Layers,
  Lightbulb,
  Link2,
  Loader2,
  MessageSquare,
  Network,
  Puzzle,
  Rocket,
  Search,
  Settings,
  Sparkles,
  Target,
  TrendingUp,
  Wand2,
  Workflow,
  Zap
} from 'lucide-react';
import { motion, AnimatePresence } from 'framer-motion';
import { cn } from '@/lib/utils';

// Advanced Types
interface MCPCapability {
  id: string;
  name: string;
  type: 'tool' | 'resource' | 'prompt' | 'workflow';
  description: string;
  schema?: any;
  complexity: number;
  performance_score: number;
  reliability: number;
  dependencies: string[];
  output_compatibility: string[];
  semantic_tags: string[];
  ai_rating: number;
}

interface MCPServer {
  id: string;
  name: string;
  url: string;
  type: 'local' | 'remote' | 'distributed';
  status: 'active' | 'inactive' | 'degraded' | 'overloaded';
  capabilities: MCPCapability[];
  performance_metrics: PerformanceMetrics;
  health_score: number;
  load_factor: number;
  tenant_id?: string;
  geo_location?: GeoLocation;
  version: string;
  ai_classification: AIClassification;
}

interface PerformanceMetrics {
  latency_p50: number;
  latency_p95: number;
  latency_p99: number;
  throughput_rps: number;
  error_rate: number;
  cpu_usage: number;
  memory_usage: number;
  network_io: number;
  concurrent_connections: number;
}

interface GeoLocation {
  region: string;
  availability_zone: string;
  coordinates: [number, number];
}

interface AIClassification {
  primary_domain: string;
  secondary_domains: string[];
  confidence_score: number;
  recommended_use_cases: string[];
  complementary_servers: string[];
}

interface WorkflowNode {
  id: string;
  type: 'server' | 'condition' | 'aggregator' | 'transformer';
  server_id?: string;
  capability_id?: string;
  position: { x: number; y: number };
  config: any;
  connections: string[];
}

interface CompositeWorkflow {
  id: string;
  name: string;
  description: string;
  nodes: WorkflowNode[];
  expected_performance: PerformanceMetrics;
  created_by: 'user' | 'ai' | 'auto_optimization';
  success_rate: number;
  usage_count: number;
}

interface RecommendationEngine {
  server_recommendations: ServerRecommendation[];
  capability_compositions: CapabilityComposition[];
  optimization_suggestions: OptimizationSuggestion[];
  predictive_insights: PredictiveInsight[];
}

interface ServerRecommendation {
  server_id: string;
  confidence: number;
  reason: string;
  use_case: string;
  estimated_improvement: number;
}

interface CapabilityComposition {
  composition_id: string;
  capabilities: string[];
  synergy_score: number;
  use_cases: string[];
  performance_prediction: PerformanceMetrics;
}

interface OptimizationSuggestion {
  type: 'load_balancing' | 'caching' | 'scaling' | 'migration';
  priority: 'low' | 'medium' | 'high' | 'critical';
  description: string;
  expected_benefit: string;
  implementation_effort: number;
}

interface PredictiveInsight {
  type: 'capacity_warning' | 'performance_degradation' | 'failure_prediction';
  confidence: number;
  timeline: string;
  description: string;
  recommended_actions: string[];
}

// Main Orchestration Engine Component
export default function MCPOrchestrationEngine() {
  const [servers, setServers] = useState<MCPServer[]>([]);
  const [workflows, setWorkflows] = useState<CompositeWorkflow[]>([]);
  const [recommendations, setRecommendations] = useState<RecommendationEngine | null>(null);
  const [selectedWorkflow, setSelectedWorkflow] = useState<string | null>(null);
  const [orchestrationMode, setOrchestrationMode] = useState<'manual' | 'ai_assisted' | 'autonomous'>('ai_assisted');
  const [loading, setLoading] = useState(false);
  const [activeTab, setActiveTab] = useState('overview');
  const [searchQuery, setSearchQuery] = useState('');
  const [filters, setFilters] = useState({
    domain: 'all',
    performance_threshold: [0, 100],
    load_threshold: [0, 100],
    health_threshold: [0, 100]
  });

  // AI-powered features
  const [aiInsights, setAiInsights] = useState<any>(null);
  const [autoOptimization, setAutoOptimization] = useState(true);
  const [semanticSearch, setSemanticSearch] = useState('');
  const [intelligentRouting, setIntelligentRouting] = useState(true);

  // Load data and start AI analysis
  useEffect(() => {
    loadServersAndWorkflows();
    startAIAnalysis();
    
    const interval = setInterval(() => {
      updateMetrics();
      if (autoOptimization) {
        runAutoOptimization();
      }
    }, 30000);

    return () => clearInterval(interval);
  }, [autoOptimization]);

  const loadServersAndWorkflows = async () => {
    setLoading(true);
    try {
      const [serversRes, workflowsRes] = await Promise.all([
        fetch('/api/mcp/orchestration/servers'),
        fetch('/api/mcp/orchestration/workflows')
      ]);

      const serversData = await serversRes.json();
      const workflowsData = await workflowsRes.json();

      setServers(serversData.servers || []);
      setWorkflows(workflowsData.workflows || []);
    } catch (error) {
      console.error('Failed to load orchestration data:', error);
    } finally {
      setLoading(false);
    }
  };

  const startAIAnalysis = async () => {
    try {
      const response = await fetch('/api/mcp/ai/analyze', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          servers: servers.map(s => s.id),
          analysis_type: 'comprehensive'
        })
      });

      if (response.ok) {
        const insights = await response.json();
        setAiInsights(insights);
        generateRecommendations(insights);
      }
    } catch (error) {
      console.error('AI analysis failed:', error);
    }
  };

  const generateRecommendations = (insights: any) => {
    // Mock AI-generated recommendations
    const recommendations: RecommendationEngine = {
      server_recommendations: [
        {
          server_id: 'stripe_server',
          confidence: 0.92,
          reason: 'High performance for payment workflows, excellent uptime',
          use_case: 'Financial transaction processing',
          estimated_improvement: 23
        },
        {
          server_id: 'openai_server',
          confidence: 0.87,
          reason: 'Advanced NLP capabilities, semantic understanding',
          use_case: 'Text analysis and generation workflows',
          estimated_improvement: 31
        }
      ],
      capability_compositions: [
        {
          composition_id: 'nlp_payment_flow',
          capabilities: ['text_analysis', 'payment_processing', 'fraud_detection'],
          synergy_score: 0.89,
          use_cases: ['Intelligent payment verification', 'Transaction summarization'],
          performance_prediction: {
            latency_p50: 120,
            latency_p95: 280,
            latency_p99: 450,
            throughput_rps: 1200,
            error_rate: 0.02,
            cpu_usage: 35,
            memory_usage: 42,
            network_io: 15,
            concurrent_connections: 150
          }
        }
      ],
      optimization_suggestions: [
        {
          type: 'load_balancing',
          priority: 'high',
          description: 'Implement intelligent load balancing for high-traffic capabilities',
          expected_benefit: 'Reduce latency by 15-20%, improve reliability',
          implementation_effort: 3
        },
        {
          type: 'caching',
          priority: 'medium',
          description: 'Add semantic caching for frequently used resource calls',
          expected_benefit: 'Reduce server load by 30%, faster response times',
          implementation_effort: 2
        }
      ],
      predictive_insights: [
        {
          type: 'capacity_warning',
          confidence: 0.78,
          timeline: '2-3 hours',
          description: 'Payment processing server approaching capacity limits',
          recommended_actions: ['Scale horizontally', 'Enable auto-scaling', 'Route traffic to backup servers']
        }
      ]
    };

    setRecommendations(recommendations);
  };

  const updateMetrics = async () => {
    try {
      const response = await fetch('/api/mcp/orchestration/metrics');
      if (response.ok) {
        const metrics = await response.json();
        updateServerMetrics(metrics);
      }
    } catch (error) {
      console.error('Failed to update metrics:', error);
    }
  };

  const updateServerMetrics = (metrics: any) => {
    setServers(prev => prev.map(server => ({
      ...server,
      performance_metrics: metrics[server.id] || server.performance_metrics,
      health_score: calculateHealthScore(metrics[server.id] || server.performance_metrics),
      load_factor: calculateLoadFactor(metrics[server.id] || server.performance_metrics)
    })));
  };

  const calculateHealthScore = (metrics: PerformanceMetrics): number => {
    const latencyScore = Math.max(0, 100 - (metrics.latency_p95 / 10));
    const errorScore = Math.max(0, 100 - (metrics.error_rate * 1000));
    const cpuScore = Math.max(0, 100 - metrics.cpu_usage);
    
    return Math.round((latencyScore + errorScore + cpuScore) / 3);
  };

  const calculateLoadFactor = (metrics: PerformanceMetrics): number => {
    return Math.round((metrics.cpu_usage + metrics.memory_usage) / 2);
  };

  const runAutoOptimization = async () => {
    try {
      await fetch('/api/mcp/orchestration/optimize', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          mode: orchestrationMode,
          auto_scale: true,
          load_balance: intelligentRouting
        })
      });
    } catch (error) {
      console.error('Auto-optimization failed:', error);
    }
  };

  const createCompositeWorkflow = async (capabilities: string[]) => {
    try {
      const response = await fetch('/api/mcp/orchestration/workflows', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          capabilities,
          optimization_level: 'high',
          auto_generate: true
        })
      });

      if (response.ok) {
        await loadServersAndWorkflows();
      }
    } catch (error) {
      console.error('Failed to create workflow:', error);
    }
  };

  // Semantic search using AI
  const performSemanticSearch = useCallback(async (query: string) => {
    if (!query.trim()) return servers;

    try {
      const response = await fetch('/api/mcp/ai/semantic-search', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ query, servers: servers.map(s => s.id) })
      });

      if (response.ok) {
        const results = await response.json();
        return servers.filter(s => results.relevant_servers.includes(s.id));
      }
    } catch (error) {
      console.error('Semantic search failed:', error);
    }

    return servers;
  }, [servers]);

  // Filtered and sorted servers
  const filteredServers = useMemo(() => {
    return servers.filter(server => {
      const matchesSearch = !searchQuery || 
        server.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
        server.ai_classification.primary_domain.toLowerCase().includes(searchQuery.toLowerCase()) ||
        server.capabilities.some(cap => 
          cap.name.toLowerCase().includes(searchQuery.toLowerCase()) ||
          cap.semantic_tags.some(tag => tag.toLowerCase().includes(searchQuery.toLowerCase()))
        );

      const matchesFilters = 
        (filters.domain === 'all' || server.ai_classification.primary_domain === filters.domain) &&
        server.health_score >= filters.health_threshold[0] &&
        server.health_score <= filters.health_threshold[1] &&
        server.load_factor >= filters.load_threshold[0] &&
        server.load_factor <= filters.load_threshold[1];

      return matchesSearch && matchesFilters;
    });
  }, [servers, searchQuery, filters]);

  // AI-powered server clustering
  const serverClusters = useMemo(() => {
    const clusters = new Map<string, MCPServer[]>();
    
    filteredServers.forEach(server => {
      const domain = server.ai_classification.primary_domain;
      if (!clusters.has(domain)) {
        clusters.set(domain, []);
      }
      clusters.get(domain)!.push(server);
    });

    return Array.from(clusters.entries()).map(([domain, servers]) => ({
      domain,
      servers,
      avg_performance: servers.reduce((sum, s) => sum + s.health_score, 0) / servers.length,
      total_capabilities: servers.reduce((sum, s) => sum + s.capabilities.length, 0)
    }));
  }, [filteredServers]);

  return (
    <div className="h-full flex flex-col bg-gradient-to-br from-slate-50 to-blue-50 dark:from-slate-900 dark:to-blue-900">
      {/* Advanced Header */}
      <div className="border-b bg-white/80 dark:bg-slate-800/80 backdrop-blur-sm px-6 py-4">
        <div className="flex justify-between items-center">
          <div>
            <h1 className="text-3xl font-bold flex items-center gap-3">
              <div className="relative">
                <CircuitBoard className="w-8 h-8 text-blue-600" />
                <div className="absolute -top-1 -right-1 w-3 h-3 bg-green-500 rounded-full animate-pulse" />
              </div>
              MCP Orchestration Engine
            </h1>
            <p className="text-sm text-gray-600 mt-1 flex items-center gap-2">
              <Brain className="w-4 h-4" />
              AI-Powered Distributed Model Context Protocol Management
            </p>
          </div>

          <div className="flex items-center gap-4">
            {/* Orchestration Mode Selector */}
            <div className="flex items-center gap-2">
              <Label className="text-sm font-medium">Mode:</Label>
              <select
                value={orchestrationMode}
                onChange={(e) => setOrchestrationMode(e.target.value as any)}
                className="px-3 py-2 border rounded-lg text-sm bg-white dark:bg-slate-700"
              >
                <option value="manual">Manual</option>
                <option value="ai_assisted">AI Assisted</option>
                <option value="autonomous">Autonomous</option>
              </select>
            </div>

            {/* Auto Optimization Toggle */}
            <div className="flex items-center gap-2">
              <Label htmlFor="auto-opt" className="text-sm">Auto Optimize</Label>
              <Switch
                id="auto-opt"
                checked={autoOptimization}
                onCheckedChange={setAutoOptimization}
              />
            </div>

            {/* System Status Indicator */}
            <div className="flex items-center gap-2 px-3 py-2 bg-green-100 dark:bg-green-900 rounded-lg">
              <div className="w-2 h-2 bg-green-500 rounded-full animate-pulse" />
              <span className="text-sm font-medium text-green-700 dark:text-green-300">
                System Optimal
              </span>
            </div>
          </div>
        </div>

        {/* Advanced Metrics Bar */}
        <div className="grid grid-cols-6 gap-4 mt-4">
          <MetricCard
            title="Active Servers"
            value={servers.filter(s => s.status === 'active').length}
            total={servers.length}
            icon={<Database className="w-5 h-5" />}
            trend="+3.2%"
            color="blue"
          />
          <MetricCard
            title="Avg Health Score"
            value={Math.round(servers.reduce((sum, s) => sum + s.health_score, 0) / servers.length)}
            total={100}
            icon={<Activity className="w-5 h-5" />}
            trend="+1.8%"
            color="green"
          />
          <MetricCard
            title="Total Capabilities"
            value={servers.reduce((sum, s) => sum + s.capabilities.length, 0)}
            icon={<Puzzle className="w-5 h-5" />}
            trend="+12.5%"
            color="purple"
          />
          <MetricCard
            title="Active Workflows"
            value={workflows.filter(w => w.success_rate > 0.8).length}
            total={workflows.length}
            icon={<Workflow className="w-5 h-5" />}
            trend="+5.1%"
            color="orange"
          />
          <MetricCard
            title="AI Efficiency"
            value={aiInsights?.efficiency_score || 87}
            total={100}
            icon={<Brain className="w-5 h-5" />}
            trend="+2.3%"
            color="indigo"
          />
          <MetricCard
            title="Cost Optimization"
            value={aiInsights?.cost_savings || 23}
            unit="%"
            icon={<TrendingUp className="w-5 h-5" />}
            trend="+4.7%"
            color="emerald"
          />
        </div>
      </div>

      {/* Main Content */}
      <div className="flex-1 overflow-hidden">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
          <TabsList className="w-full justify-start px-6 bg-transparent">
            <TabsTrigger value="overview" className="flex items-center gap-2">
              <BarChart3 className="w-4 h-4" />
              Intelligent Overview
            </TabsTrigger>
            <TabsTrigger value="orchestration" className="flex items-center gap-2">
              <CircuitBoard className="w-4 h-4" />
              Orchestration Matrix
            </TabsTrigger>
            <TabsTrigger value="workflows" className="flex items-center gap-2">
              <Workflow className="w-4 h-4" />
              Composite Workflows
            </TabsTrigger>
            <TabsTrigger value="ai_insights" className="flex items-center gap-2">
              <Brain className="w-4 h-4" />
              AI Insights
            </TabsTrigger>
            <TabsTrigger value="optimization" className="flex items-center gap-2">
              <Rocket className="w-4 h-4" />
              Auto Optimization
            </TabsTrigger>
          </TabsList>

          <div className="flex-1 overflow-y-auto">
            <TabsContent value="overview" className="h-full p-6 space-y-6">
              <IntelligentOverview 
                servers={filteredServers}
                clusters={serverClusters}
                recommendations={recommendations}
                onSearchChange={setSemanticSearch}
                onFiltersChange={setFilters}
              />
            </TabsContent>

            <TabsContent value="orchestration" className="h-full p-6">
              <OrchestrationMatrix 
                servers={filteredServers}
                mode={orchestrationMode}
                onCreateWorkflow={createCompositeWorkflow}
              />
            </TabsContent>

            <TabsContent value="workflows" className="h-full p-6">
              <CompositeWorkflows 
                workflows={workflows}
                servers={servers}
                selectedWorkflow={selectedWorkflow}
                onSelectWorkflow={setSelectedWorkflow}
              />
            </TabsContent>

            <TabsContent value="ai_insights" className="h-full p-6">
              <AIInsightsPanel 
                insights={aiInsights}
                recommendations={recommendations}
                servers={servers}
              />
            </TabsContent>

            <TabsContent value="optimization" className="h-full p-6">
              <AutoOptimizationPanel 
                enabled={autoOptimization}
                mode={orchestrationMode}
                recommendations={recommendations}
                onToggle={setAutoOptimization}
              />
            </TabsContent>
          </div>
        </Tabs>
      </div>
    </div>
  );
}

// Helper Components
function MetricCard({ title, value, total, unit, icon, trend, color }: any) {
  const percentage = total ? (value / total) * 100 : value;
  
  const colorClasses = {
    blue: 'text-blue-600 bg-blue-100',
    green: 'text-green-600 bg-green-100',
    purple: 'text-purple-600 bg-purple-100',
    orange: 'text-orange-600 bg-orange-100',
    indigo: 'text-indigo-600 bg-indigo-100',
    emerald: 'text-emerald-600 bg-emerald-100'
  };

  return (
    <Card className="relative overflow-hidden">
      <CardContent className="p-4">
        <div className="flex items-center justify-between">
          <div className={cn("p-2 rounded-lg", colorClasses[color])}>
            {icon}
          </div>
          {trend && (
            <Badge variant="secondary" className="text-xs text-green-600">
              {trend}
            </Badge>
          )}
        </div>
        <div className="mt-3">
          <p className="text-2xl font-bold">
            {value}{unit}
            {total && <span className="text-sm text-gray-500">/{total}</span>}
          </p>
          <p className="text-sm text-gray-600">{title}</p>
          {total && (
            <Progress value={percentage} className="mt-2 h-1" />
          )}
        </div>
      </CardContent>
    </Card>
  );
}

function IntelligentOverview({ servers, clusters, recommendations, onSearchChange, onFiltersChange }: any) {
  return (
    <div className="space-y-6">
      {/* AI-Powered Search */}
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Search className="w-5 h-5" />
            Semantic Server Discovery
          </CardTitle>
          <CardDescription>
            Use natural language to find the perfect MCP servers for your needs
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="flex gap-4">
            <Input
              placeholder="e.g., 'servers good for payment processing with low latency'"
              onChange={(e) => onSearchChange(e.target.value)}
              className="flex-1"
            />
            <Button>
              <Sparkles className="w-4 h-4 mr-2" />
              AI Search
            </Button>
          </div>
        </CardContent>
      </Card>

      {/* Server Clusters */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {clusters.map((cluster: any) => (
          <ServerClusterCard key={cluster.domain} cluster={cluster} />
        ))}
      </div>

      {/* AI Recommendations */}
      {recommendations && (
        <Card>
          <CardHeader>
            <CardTitle className="flex items-center gap-2">
              <Lightbulb className="w-5 h-5" />
              AI Recommendations
            </CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              {recommendations.server_recommendations.map((rec, idx) => (
                <div key={idx} className="flex items-center justify-between p-3 bg-gradient-to-r from-blue-50 to-purple-50 rounded-lg">
                  <div>
                    <p className="font-medium">{rec.reason}</p>
                    <p className="text-sm text-gray-600">{rec.use_case}</p>
                  </div>
                  <div className="text-right">
                    <Badge className="bg-green-100 text-green-700">
                      {rec.confidence * 100}% confidence
                    </Badge>
                    <p className="text-sm text-green-600 mt-1">
                      +{rec.estimated_improvement}% improvement
                    </p>
                  </div>
                </div>
              ))}
            </div>
          </CardContent>
        </Card>
      )}
    </div>
  );
}

function ServerClusterCard({ cluster }: any) {
  const [expanded, setExpanded] = useState(false);

  return (
    <Card className="cursor-pointer hover:shadow-lg transition-shadow">
      <CardHeader onClick={() => setExpanded(!expanded)}>
        <CardTitle className="flex items-center justify-between">
          <span className="capitalize">{cluster.domain}</span>
          <div className="flex items-center gap-2">
            <Badge variant="outline">{cluster.servers.length} servers</Badge>
            {expanded ? <ChevronUp className="w-4 h-4" /> : <ChevronDown className="w-4 h-4" />}
          </div>
        </CardTitle>
        <CardDescription>
          Avg Performance: {cluster.avg_performance.toFixed(1)}% • {cluster.total_capabilities} capabilities
        </CardDescription>
      </CardHeader>
      
      <AnimatePresence>
        {expanded && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
          >
            <CardContent>
              <div className="space-y-2">
                {cluster.servers.map((server: MCPServer) => (
                  <div key={server.id} className="flex items-center justify-between p-2 bg-gray-50 rounded">
                    <span className="font-medium">{server.name}</span>
                    <div className="flex items-center gap-2">
                      <Progress value={server.health_score} className="w-16 h-2" />
                      <span className="text-xs">{server.health_score}%</span>
                    </div>
                  </div>
                ))}
              </div>
            </CardContent>
          </motion.div>
        )}
      </AnimatePresence>
    </Card>
  );
}

function OrchestrationMatrix({ servers, mode, onCreateWorkflow }: any) {
  const [selectedCapabilities, setSelectedCapabilities] = useState<string[]>([]);

  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Network className="w-5 h-5" />
            Capability Orchestration Matrix
          </CardTitle>
          <CardDescription>
            Visualize and compose server capabilities into intelligent workflows
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="grid grid-cols-4 gap-4">
            {servers.map((server: MCPServer) => (
              <div key={server.id} className="border rounded-lg p-4">
                <h4 className="font-semibold mb-2">{server.name}</h4>
                <div className="space-y-1">
                  {server.capabilities.slice(0, 3).map((cap: MCPCapability) => (
                    <div key={cap.id} 
                         className={cn(
                           "p-2 rounded text-xs cursor-pointer",
                           selectedCapabilities.includes(cap.id) 
                             ? "bg-blue-100 border-blue-300" 
                             : "bg-gray-50 hover:bg-gray-100"
                         )}
                         onClick={() => {
                           setSelectedCapabilities(prev => 
                             prev.includes(cap.id) 
                               ? prev.filter(id => id !== cap.id)
                               : [...prev, cap.id]
                           );
                         }}>
                      {cap.name}
                    </div>
                  ))}
                </div>
              </div>
            ))}
          </div>
          
          {selectedCapabilities.length > 1 && (
            <div className="mt-4 p-4 bg-blue-50 rounded-lg">
              <div className="flex items-center justify-between">
                <div>
                  <p className="font-medium">Selected Capabilities: {selectedCapabilities.length}</p>
                  <p className="text-sm text-gray-600">Ready to create composite workflow</p>
                </div>
                <Button onClick={() => onCreateWorkflow(selectedCapabilities)}>
                  <Wand2 className="w-4 h-4 mr-2" />
                  Create AI Workflow
                </Button>
              </div>
            </div>
          )}
        </CardContent>
      </Card>
    </div>
  );
}

function CompositeWorkflows({ workflows, servers, selectedWorkflow, onSelectWorkflow }: any) {
  return (
    <div className="space-y-6">
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {workflows.map((workflow: CompositeWorkflow) => (
          <Card key={workflow.id} 
                className={cn(
                  "cursor-pointer transition-all",
                  selectedWorkflow === workflow.id && "ring-2 ring-blue-500"
                )}
                onClick={() => onSelectWorkflow(workflow.id)}>
            <CardHeader>
              <CardTitle className="flex items-center justify-between">
                <span>{workflow.name}</span>
                <Badge className={workflow.created_by === 'ai' ? 'bg-purple-100 text-purple-700' : 'bg-gray-100'}>
                  {workflow.created_by}
                </Badge>
              </CardTitle>
              <CardDescription>{workflow.description}</CardDescription>
            </CardHeader>
            <CardContent>
              <div className="space-y-3">
                <div className="flex justify-between text-sm">
                  <span>Success Rate:</span>
                  <span className="font-medium">{(workflow.success_rate * 100).toFixed(1)}%</span>
                </div>
                <Progress value={workflow.success_rate * 100} />
                
                <div className="flex justify-between text-sm">
                  <span>Usage Count:</span>
                  <span className="font-medium">{workflow.usage_count}</span>
                </div>
                
                <div className="flex justify-between text-sm">
                  <span>Nodes:</span>
                  <span className="font-medium">{workflow.nodes.length}</span>
                </div>
              </div>
            </CardContent>
          </Card>
        ))}
      </div>
    </div>
  );
}

function AIInsightsPanel({ insights, recommendations, servers }: any) {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center gap-2">
            <Brain className="w-5 h-5" />
            Predictive Analytics
          </CardTitle>
        </CardHeader>
        <CardContent>
          {recommendations?.predictive_insights?.map((insight: PredictiveInsight, idx: number) => (
            <div key={idx} className="p-4 border rounded-lg mb-4">
              <div className="flex items-center justify-between mb-2">
                <h4 className="font-semibold">{insight.type.replace('_', ' ').toUpperCase()}</h4>
                <Badge className={insight.confidence > 0.8 ? 'bg-red-100 text-red-700' : 'bg-yellow-100 text-yellow-700'}>
                  {(insight.confidence * 100).toFixed(0)}% confidence
                </Badge>
              </div>
              <p className="text-sm text-gray-600 mb-3">{insight.description}</p>
              <div className="space-y-1">
                {insight.recommended_actions.map((action, actionIdx) => (
                  <div key={actionIdx} className="flex items-center gap-2 text-sm">
                    <Target className="w-3 h-3 text-blue-500" />
                    <span>{action}</span>
                  </div>
                ))}
              </div>
            </div>
          ))}
        </CardContent>
      </Card>
    </div>
  );
}

function AutoOptimizationPanel({ enabled, mode, recommendations, onToggle }: any) {
  return (
    <div className="space-y-6">
      <Card>
        <CardHeader>
          <CardTitle className="flex items-center justify-between">
            <span className="flex items-center gap-2">
              <Rocket className="w-5 h-5" />
              Autonomous Optimization
            </span>
            <Switch checked={enabled} onCheckedChange={onToggle} />
          </CardTitle>
          <CardDescription>
            AI-driven automatic optimization of server allocation and load balancing
          </CardDescription>
        </CardHeader>
        <CardContent>
          {recommendations?.optimization_suggestions?.map((suggestion: OptimizationSuggestion, idx: number) => (
            <div key={idx} className="p-4 border rounded-lg mb-4">
              <div className="flex items-center justify-between mb-2">
                <h4 className="font-semibold capitalize">{suggestion.type.replace('_', ' ')}</h4>
                <Badge className={
                  suggestion.priority === 'critical' ? 'bg-red-100 text-red-700' :
                  suggestion.priority === 'high' ? 'bg-orange-100 text-orange-700' :
                  suggestion.priority === 'medium' ? 'bg-yellow-100 text-yellow-700' :
                  'bg-gray-100 text-gray-700'
                }>
                  {suggestion.priority}
                </Badge>
              </div>
              <p className="text-sm text-gray-600 mb-2">{suggestion.description}</p>
              <p className="text-sm text-green-600 mb-3">Expected: {suggestion.expected_benefit}</p>
              <div className="flex items-center justify-between">
                <div className="flex items-center gap-2">
                  <span className="text-xs text-gray-500">Implementation Effort:</span>
                  <Progress value={suggestion.implementation_effort * 20} className="w-20 h-2" />
                </div>
                <Button size="sm" variant="outline">
                  Apply Optimization
                </Button>
              </div>
            </div>
          ))}
        </CardContent>
      </Card>
    </div>
  );
}