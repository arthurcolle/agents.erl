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
    <div className="h-full flex flex-col bg-gradient-to-br from-slate-50 via-blue-50 to-indigo-50 dark:from-slate-900 dark:via-slate-800 dark:to-blue-900">
      {/* Enhanced Header with Glass Morphism */}
      <div className="border-b border-white/20 bg-white/70 dark:bg-slate-800/70 backdrop-blur-xl shadow-xl px-8 py-6">
        <div className="flex justify-between items-center">
          <div className="space-y-2">
            <h1 className="text-4xl font-bold flex items-center gap-4 text-transparent bg-clip-text bg-gradient-to-r from-blue-600 via-purple-600 to-indigo-600">
              <div className="relative p-2 rounded-2xl bg-gradient-to-br from-blue-500 to-purple-600 shadow-lg">
                <CircuitBoard className="w-8 h-8 text-white" />
                <div className="absolute -top-1 -right-1 w-4 h-4 bg-green-400 rounded-full animate-pulse shadow-lg" />
                <div className="absolute -top-1 -right-1 w-4 h-4 bg-green-400 rounded-full animate-ping" />
              </div>
              MCP Orchestration Engine
            </h1>
            <p className="text-sm text-gray-600 dark:text-gray-300 flex items-center gap-2 font-medium">
              <Brain className="w-4 h-4 text-purple-500" />
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

        {/* Enhanced Metrics Bar with Glass Cards */}
        <div className="grid grid-cols-6 gap-6 mt-6">
          <MetricCard
            title="Active Servers"
            value={servers.filter(s => s.status === 'active').length || 0}
            total={servers.length || 8}
            icon={<Database className="w-5 h-5" />}
            trend="+2.3%"
            color="blue"
            description="Healthy endpoints"
          />
          <MetricCard
            title="Avg Health Score"
            value={Math.round(servers.reduce((sum, s) => sum + (s.health_score || 85), 0) / (servers.length || 1)) || 87}
            total={100}
            icon={<Activity className="w-5 h-5" />}
            trend="+1.8%"
            color="green"
            description="System performance"
          />
          <MetricCard
            title="Total Capabilities"
            value={servers.reduce((sum, s) => sum + (s.capabilities?.length || 0), 0) || 0}
            icon={<Puzzle className="w-5 h-5" />}
            trend="+12.5%"
            color="purple"
            description="Available tools"
          />
          <MetricCard
            title="Active Workflows"
            value={workflows.filter(w => w.success_rate > 0.8).length || 0}
            total={workflows.length || 0}
            icon={<Workflow className="w-5 h-5" />}
            trend="+5.1%"
            color="orange"
            description="Running processes"
          />
          <MetricCard
            title="AI Efficiency"
            value={aiInsights?.efficiency_score || 87}
            total={100}
            icon={<Brain className="w-5 h-5" />}
            trend="+2.3%"
            color="indigo"
            description="Model optimization"
          />
          <MetricCard
            title="Cost Optimization"
            value={aiInsights?.cost_savings || 23}
            unit="%"
            icon={<TrendingUp className="w-5 h-5" />}
            trend="+4.7%"
            color="emerald"
            description="Resource efficiency"
          />
        </div>
      </div>

      {/* Main Content */}
      <div className="flex-1 overflow-hidden">
        <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
          <TabsList className="w-full justify-start px-6 bg-white/50 dark:bg-slate-800/50 backdrop-blur-sm border border-white/30 shadow-lg rounded-xl">
            <TabsTrigger value="overview" className="flex items-center gap-2 data-[state=active]:bg-gradient-to-r data-[state=active]:from-blue-500 data-[state=active]:to-purple-600 data-[state=active]:text-white transition-all duration-300">
              <BarChart3 className="w-4 h-4" />
              Intelligent Overview
            </TabsTrigger>
            <TabsTrigger value="orchestration" className="flex items-center gap-2 data-[state=active]:bg-gradient-to-r data-[state=active]:from-green-500 data-[state=active]:to-emerald-600 data-[state=active]:text-white transition-all duration-300">
              <CircuitBoard className="w-4 h-4" />
              Orchestration Matrix
            </TabsTrigger>
            <TabsTrigger value="workflows" className="flex items-center gap-2 data-[state=active]:bg-gradient-to-r data-[state=active]:from-purple-500 data-[state=active]:to-pink-600 data-[state=active]:text-white transition-all duration-300">
              <Workflow className="w-4 h-4" />
              Composite Workflows
            </TabsTrigger>
            <TabsTrigger value="ai_insights" className="flex items-center gap-2 data-[state=active]:bg-gradient-to-r data-[state=active]:from-indigo-500 data-[state=active]:to-blue-600 data-[state=active]:text-white transition-all duration-300">
              <Brain className="w-4 h-4" />
              AI Insights
            </TabsTrigger>
            <TabsTrigger value="optimization" className="flex items-center gap-2 data-[state=active]:bg-gradient-to-r data-[state=active]:from-orange-500 data-[state=active]:to-red-600 data-[state=active]:text-white transition-all duration-300">
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
function MetricCard({ title, value, total, unit, icon, trend, color, description }: any) {
  const percentage = total ? (value / total) * 100 : value;
  
  const colorClasses = {
    blue: 'text-blue-600 bg-gradient-to-br from-blue-500/10 to-blue-600/20 border-blue-200/50',
    green: 'text-green-600 bg-gradient-to-br from-green-500/10 to-green-600/20 border-green-200/50',
    purple: 'text-purple-600 bg-gradient-to-br from-purple-500/10 to-purple-600/20 border-purple-200/50',
    orange: 'text-orange-600 bg-gradient-to-br from-orange-500/10 to-orange-600/20 border-orange-200/50',
    indigo: 'text-indigo-600 bg-gradient-to-br from-indigo-500/10 to-indigo-600/20 border-indigo-200/50',
    emerald: 'text-emerald-600 bg-gradient-to-br from-emerald-500/10 to-emerald-600/20 border-emerald-200/50'
  };

  const bgGradients = {
    blue: 'from-blue-400/20 to-blue-600/30',
    green: 'from-green-400/20 to-green-600/30',
    purple: 'from-purple-400/20 to-purple-600/30',
    orange: 'from-orange-400/20 to-orange-600/30',
    indigo: 'from-indigo-400/20 to-indigo-600/30',
    emerald: 'from-emerald-400/20 to-emerald-600/30'
  };

  return (
    <Card className="relative overflow-hidden bg-white/60 dark:bg-slate-800/60 backdrop-blur-sm border-white/20 shadow-xl hover:shadow-2xl transition-all duration-300 hover:scale-105">
      <div className={`absolute inset-0 bg-gradient-to-br ${bgGradients[color]} opacity-50`} />
      <CardContent className="relative p-5">
        <div className="flex items-center justify-between mb-4">
          <div className={cn("p-3 rounded-xl shadow-lg", colorClasses[color])}>
            {icon}
          </div>
          {trend && (
            <Badge className="text-xs bg-green-100/80 text-green-700 border-green-200/50 shadow-sm">
              {trend}
            </Badge>
          )}
        </div>
        <div className="space-y-2">
          <div className="flex items-baseline gap-1">
            <p className="text-3xl font-bold text-gray-800 dark:text-white">
              {value}{unit}
            </p>
            {total && <span className="text-sm text-gray-500 dark:text-gray-400">/{total}</span>}
          </div>
          <p className="text-sm font-semibold text-gray-700 dark:text-gray-200">{title}</p>
          {description && (
            <p className="text-xs text-gray-500 dark:text-gray-400">{description}</p>
          )}
          {total && (
            <Progress value={percentage} className="mt-3 h-2 bg-gray-200/50" />
          )}
        </div>
      </CardContent>
    </Card>
  );
}

function IntelligentOverview({ servers, clusters, recommendations, onSearchChange, onFiltersChange }: any) {
  return (
    <div className="space-y-8">
      {/* Enhanced AI-Powered Search */}
      <Card className="bg-gradient-to-r from-blue-50 via-purple-50 to-indigo-50 dark:from-blue-900/20 dark:via-purple-900/20 dark:to-indigo-900/20 border-blue-200/50 shadow-xl">
        <CardHeader className="pb-4">
          <CardTitle className="flex items-center gap-3 text-xl">
            <div className="p-2 rounded-xl bg-gradient-to-br from-blue-500 to-purple-600 shadow-lg">
              <Search className="w-5 h-5 text-white" />
            </div>
            Semantic Server Discovery
            <Badge className="bg-purple-100 text-purple-700 border-purple-200">AI-Powered</Badge>
          </CardTitle>
          <CardDescription className="text-gray-600 dark:text-gray-300">
            Use natural language to find the perfect MCP servers for your needs
          </CardDescription>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            <div className="flex gap-4">
              <div className="relative flex-1">
                <Search className="absolute left-4 top-1/2 transform -translate-y-1/2 w-4 h-4 text-gray-400" />
                <Input
                  placeholder="e.g., 'servers good for payment processing with low latency'"
                  onChange={(e) => onSearchChange(e.target.value)}
                  className="pl-12 h-12 bg-white/80 backdrop-blur-sm border-white/50 shadow-sm text-gray-700 placeholder:text-gray-400"
                />
              </div>
              <Button size="lg" className="h-12 px-8 bg-gradient-to-r from-blue-600 to-purple-600 hover:from-blue-700 hover:to-purple-700 shadow-lg hover:shadow-xl transition-all">
                <Sparkles className="w-4 h-4 mr-2" />
                AI Search
              </Button>
            </div>
            <div className="flex flex-wrap gap-2">
              <Badge variant="outline" className="cursor-pointer hover:bg-blue-50">payment processing</Badge>
              <Badge variant="outline" className="cursor-pointer hover:bg-blue-50">low latency</Badge>
              <Badge variant="outline" className="cursor-pointer hover:bg-blue-50">high throughput</Badge>
              <Badge variant="outline" className="cursor-pointer hover:bg-blue-50">machine learning</Badge>
            </div>
          </div>
        </CardContent>
      </Card>

      {/* Server Clusters */}
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
        {clusters.map((cluster: any) => (
          <ServerClusterCard key={cluster.domain} cluster={cluster} />
        ))}
      </div>

      {/* Enhanced AI Recommendations */}
      {recommendations && (
        <Card className="bg-gradient-to-r from-amber-50 via-orange-50 to-red-50 dark:from-amber-900/20 dark:via-orange-900/20 dark:to-red-900/20 border-amber-200/50 shadow-xl">
          <CardHeader>
            <CardTitle className="flex items-center gap-3 text-xl">
              <div className="p-2 rounded-xl bg-gradient-to-br from-amber-500 to-orange-600 shadow-lg">
                <Lightbulb className="w-5 h-5 text-white" />
              </div>
              AI Recommendations
              <Badge className="bg-gradient-to-r from-green-500 to-emerald-600 text-white border-0 shadow-sm">
                {recommendations.server_recommendations.length} insights
              </Badge>
            </CardTitle>
            <CardDescription>Intelligent suggestions based on performance analysis and usage patterns</CardDescription>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              {recommendations.server_recommendations.map((rec, idx) => (
                <motion.div 
                  key={idx}
                  initial={{ opacity: 0, y: 20 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ delay: idx * 0.1 }}
                  className="group relative overflow-hidden rounded-xl bg-white/80 dark:bg-slate-800/80 backdrop-blur-sm border border-white/50 shadow-lg hover:shadow-xl transition-all duration-300 hover:scale-102"
                >
                  <div className="absolute inset-0 bg-gradient-to-r from-blue-500/5 to-purple-500/5" />
                  <div className="relative p-6">
                    <div className="flex items-start justify-between">
                      <div className="flex-1">
                        <div className="flex items-center gap-2 mb-2">
                          <div className="w-2 h-2 rounded-full bg-gradient-to-r from-blue-500 to-purple-600" />
                          <p className="font-semibold text-gray-800 dark:text-white">{rec.reason}</p>
                        </div>
                        <p className="text-sm text-gray-600 dark:text-gray-300 mb-3">{rec.use_case}</p>
                        <div className="flex items-center gap-4">
                          <Badge className="bg-green-100 text-green-700 border-green-200/50">
                            {Math.round(rec.confidence * 100)}% confidence
                          </Badge>
                          <Badge className="bg-blue-100 text-blue-700 border-blue-200/50">
                            +{rec.estimated_improvement}% improvement
                          </Badge>
                        </div>
                      </div>
                      <Button size="sm" variant="outline" className="ml-4 hover:bg-blue-50 transition-colors">
                        Apply
                      </Button>
                    </div>
                  </div>
                </motion.div>
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

  const getDomainColor = (domain: string) => {
    const colors = {
      'payment': 'from-green-400 to-emerald-600',
      'ai': 'from-purple-400 to-indigo-600',
      'analytics': 'from-blue-400 to-cyan-600',
      'storage': 'from-orange-400 to-red-600',
      'communication': 'from-pink-400 to-rose-600',
      'default': 'from-gray-400 to-slate-600'
    };
    return colors[domain as keyof typeof colors] || colors.default;
  };

  return (
    <Card className="group cursor-pointer hover:shadow-2xl transition-all duration-300 hover:scale-105 bg-white/60 dark:bg-slate-800/60 backdrop-blur-sm border-white/30 overflow-hidden">
      <div className={`absolute inset-0 bg-gradient-to-br ${getDomainColor(cluster.domain)} opacity-5 group-hover:opacity-10 transition-opacity`} />
      <CardHeader onClick={() => setExpanded(!expanded)} className="relative">
        <CardTitle className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <div className={`w-3 h-3 rounded-full bg-gradient-to-r ${getDomainColor(cluster.domain)}`} />
            <span className="capitalize font-bold text-gray-800 dark:text-white">{cluster.domain}</span>
          </div>
          <div className="flex items-center gap-2">
            <Badge className="bg-blue-100 text-blue-700 border-blue-200/50">
              {cluster.servers.length} servers
            </Badge>
            <motion.div
              animate={{ rotate: expanded ? 180 : 0 }}
              transition={{ duration: 0.2 }}
            >
              <ChevronDown className="w-4 h-4 text-gray-500" />
            </motion.div>
          </div>
        </CardTitle>
        <CardDescription className="text-gray-600 dark:text-gray-300">
          <div className="flex items-center gap-4">
            <span>Avg Performance: <strong>{cluster.avg_performance.toFixed(1)}%</strong></span>
            <span>•</span>
            <span><strong>{cluster.total_capabilities}</strong> capabilities</span>
          </div>
        </CardDescription>
      </CardHeader>
      
      <AnimatePresence>
        {expanded && (
          <motion.div
            initial={{ height: 0, opacity: 0 }}
            animate={{ height: 'auto', opacity: 1 }}
            exit={{ height: 0, opacity: 0 }}
            transition={{ duration: 0.3 }}
          >
            <CardContent className="relative">
              <div className="space-y-3">
                {cluster.servers.map((server: MCPServer, idx: number) => (
                  <motion.div 
                    key={server.id}
                    initial={{ opacity: 0, x: -20 }}
                    animate={{ opacity: 1, x: 0 }}
                    transition={{ delay: idx * 0.05 }}
                    className="flex items-center justify-between p-3 bg-white/80 dark:bg-slate-700/50 rounded-lg border border-white/30 hover:bg-white/90 transition-colors"
                  >
                    <div className="flex items-center gap-3">
                      <div className={`w-2 h-2 rounded-full ${server.status === 'active' ? 'bg-green-500' : 'bg-gray-400'}`} />
                      <span className="font-medium text-gray-800 dark:text-white">{server.name}</span>
                    </div>
                    <div className="flex items-center gap-3">
                      <Progress value={server.health_score || 85} className="w-20 h-2" />
                      <span className="text-sm font-semibold text-gray-600 dark:text-gray-300 min-w-[3rem]">
                        {server.health_score || 85}%
                      </span>
                    </div>
                  </motion.div>
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