import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { FlameGraph } from './FlameGraph'
import { 
  Bot, 
  Activity, 
  Database, 
  Code, 
  Clock,
  DollarSign,
  MessageSquare,
  Zap,
  Flame
} from 'lucide-react'
import { cn } from '@/lib/utils'

interface InspectorData {
  type: 'agent' | 'message' | 'log' | 'network' | null
  data: any
}

interface RightDrawerInspectorProps {
  selectedData: InspectorData
  className?: string
}

export function RightDrawerInspector({ selectedData, className }: RightDrawerInspectorProps) {
  const [activeTab, setActiveTab] = useState('overview')

  useEffect(() => {
    // Reset to overview when data changes
    setActiveTab('overview')
  }, [selectedData])

  if (!selectedData.type || !selectedData.data) {
    return (
      <div className={cn("h-full flex items-center justify-center text-center p-4", className)}>
        <div className="text-muted-foreground">
          <Activity className="h-8 w-8 mx-auto mb-2 opacity-50" />
          <p className="text-sm">Select an agent, message, or log entry to inspect details</p>
        </div>
      </div>
    )
  }

  const renderAgentInspector = (agent: any) => (
    <Tabs value={activeTab} onValueChange={setActiveTab} className="h-full flex flex-col">
      <TabsList className="grid grid-cols-4 w-full">
        <TabsTrigger value="overview" className="text-xs">Overview</TabsTrigger>
        <TabsTrigger value="memory" className="text-xs">Memory</TabsTrigger>
        <TabsTrigger value="tools" className="text-xs">Tools</TabsTrigger>
        <TabsTrigger value="trace" className="text-xs">Trace</TabsTrigger>
      </TabsList>

      <div className="flex-1 overflow-hidden">
        <TabsContent value="overview" className="h-full m-0 p-2 overflow-y-auto">
          <div className="space-y-3">
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm flex items-center gap-2">
                  <Bot className="h-4 w-4" />
                  Agent Details
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">ID:</span>
                  <code className="text-xs">{String(agent.id).substring(0, 12)}</code>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Type:</span>
                  <Badge variant="outline">{agent.type}</Badge>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Status:</span>
                  <Badge variant={agent.status === 'active' ? 'default' : 'secondary'}>
                    {agent.status || 'unknown'}
                  </Badge>
                </div>
                {agent.model && (
                  <div className="flex justify-between text-sm">
                    <span className="text-muted-foreground">Model:</span>
                    <span>{agent.model}</span>
                  </div>
                )}
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm flex items-center gap-2">
                  <Activity className="h-4 w-4" />
                  Performance
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Uptime:</span>
                  <span>{Math.floor(Math.random() * 24)}h {Math.floor(Math.random() * 60)}m</span>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Messages:</span>
                  <span>{Math.floor(Math.random() * 1000)}</span>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Avg Response:</span>
                  <span>{(Math.random() * 2 + 0.5).toFixed(1)}s</span>
                </div>
              </CardContent>
            </Card>

            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm flex items-center gap-2">
                  <DollarSign className="h-4 w-4" />
                  Token Usage
                </CardTitle>
              </CardHeader>
              <CardContent className="space-y-2">
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Total Tokens:</span>
                  <span>{(Math.random() * 100000).toFixed(0)}</span>
                </div>
                <div className="flex justify-between text-sm">
                  <span className="text-muted-foreground">Cost Today:</span>
                  <span>${(Math.random() * 10).toFixed(2)}</span>
                </div>
              </CardContent>
            </Card>
          </div>
        </TabsContent>

        <TabsContent value="memory" className="h-full m-0 p-2 overflow-y-auto">
          <Card>
            <CardHeader className="pb-2">
              <CardTitle className="text-sm flex items-center gap-2">
                <Database className="h-4 w-4" />
                Agent Memory
              </CardTitle>
            </CardHeader>
            <CardContent>
              <div className="bg-muted/30 p-2 rounded text-xs font-mono">
                <pre className="whitespace-pre-wrap">
{JSON.stringify({
  context: "Recent conversation context...",
  tools: ["weather_tool", "search_tool"],
  preferences: { temperature: 0.7 },
  history_length: Math.floor(Math.random() * 50)
}, null, 2)}
                </pre>
              </div>
            </CardContent>
          </Card>
        </TabsContent>

        <TabsContent value="tools" className="h-full m-0 p-2 overflow-y-auto">
          <div className="space-y-2">
            {['weather_tool', 'search_tool', 'file_tool', 'calc_tool'].map((tool) => (
              <Card key={tool}>
                <CardContent className="p-3">
                  <div className="flex items-center justify-between">
                    <div className="flex items-center gap-2">
                      <Zap className="h-3 w-3" />
                      <span className="text-sm font-medium">{tool}</span>
                    </div>
                    <Badge variant="outline" className="text-xs">
                      {Math.floor(Math.random() * 50)} calls
                    </Badge>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </TabsContent>

        <TabsContent value="trace" className="h-full m-0 p-2 overflow-y-auto">
          <div className="space-y-3">
            <Card>
              <CardHeader className="pb-2">
                <CardTitle className="text-sm flex items-center gap-2">
                  <Flame className="h-4 w-4" />
                  Request Trace
                </CardTitle>
              </CardHeader>
              <CardContent>
                <FlameGraph height={200} />
              </CardContent>
            </Card>
          </div>
        </TabsContent>
      </div>
    </Tabs>
  )

  const renderMessageInspector = (message: any) => (
    <div className="space-y-3 p-2">
      <Card>
        <CardHeader className="pb-2">
          <CardTitle className="text-sm flex items-center gap-2">
            <MessageSquare className="h-4 w-4" />
            Message Details
          </CardTitle>
        </CardHeader>
        <CardContent className="space-y-2">
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Timestamp:</span>
            <span>{new Date().toLocaleTimeString()}</span>
          </div>
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Tokens:</span>
            <span>{message.tokens || Math.floor(Math.random() * 1000)}</span>
          </div>
          <div className="flex justify-between text-sm">
            <span className="text-muted-foreground">Cost:</span>
            <span>${((message.tokens || 500) * 0.002).toFixed(3)}</span>
          </div>
        </CardContent>
      </Card>

      <Card>
        <CardHeader className="pb-2">
          <CardTitle className="text-sm">Raw Content</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="bg-muted/30 p-2 rounded text-xs font-mono max-h-40 overflow-y-auto">
            {message.content || "Message content..."}
          </div>
        </CardContent>
      </Card>
    </div>
  )

  const renderGenericInspector = (data: any) => (
    <div className="p-2">
      <Card>
        <CardHeader className="pb-2">
          <CardTitle className="text-sm flex items-center gap-2">
            <Code className="h-4 w-4" />
            Raw Data
          </CardTitle>
        </CardHeader>
        <CardContent>
          <div className="bg-muted/30 p-2 rounded text-xs font-mono max-h-96 overflow-y-auto">
            <pre className="whitespace-pre-wrap">
              {JSON.stringify(data, null, 2)}
            </pre>
          </div>
        </CardContent>
      </Card>
    </div>
  )

  return (
    <div className={cn("h-full", className)}>
      {selectedData.type === 'agent' && renderAgentInspector(selectedData.data)}
      {selectedData.type === 'message' && renderMessageInspector(selectedData.data)}
      {(selectedData.type === 'log' || selectedData.type === 'network') && renderGenericInspector(selectedData.data)}
    </div>
  )
}