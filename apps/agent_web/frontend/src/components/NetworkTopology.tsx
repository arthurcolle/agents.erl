import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { RefreshCw, Maximize2, Zap, Activity, Bot, MessageSquare } from 'lucide-react'
import { cn } from '@/lib/utils'

interface NetworkTopologyProps {
  agents: Map<string, any>
  onAgentClick?: (agentId: string) => void
}

export default function NetworkTopology({ agents, onAgentClick }: NetworkTopologyProps) {
  const [selectedAgent, setSelectedAgent] = useState<string | null>(null)
  const [connections, setConnections] = useState<string[]>([])
  const [animate, setAnimate] = useState(false)

  useEffect(() => {
    // Simulate network activity
    const interval = setInterval(() => {
      setAnimate(true)
      setTimeout(() => setAnimate(false), 1000)
    }, 3000)

    return () => clearInterval(interval)
  }, [])

  const refreshTopology = () => {
    console.log('Refreshing network topology...')
    setAnimate(true)
    setTimeout(() => setAnimate(false), 1000)
  }

  const autoLayout = () => {
    console.log('Auto-layout network nodes...')
    setAnimate(true)
    setTimeout(() => setAnimate(false), 1000)
  }

  const handleAgentClick = (agentId: string) => {
    setSelectedAgent(agentId)
    onAgentClick?.(agentId)
  }

  return (
    <Card>
      <CardHeader className="flex flex-row items-center justify-between">
        <CardTitle>Network Topology</CardTitle>
        <div className="flex gap-2">
          <Button size="sm" variant="outline" onClick={refreshTopology}>
            <RefreshCw className="h-4 w-4" />
          </Button>
          <Button size="sm" variant="outline" onClick={autoLayout}>
            <Maximize2 className="h-4 w-4" />
          </Button>
        </div>
      </CardHeader>
      <CardContent>
        <div className="relative h-96 bg-muted rounded-lg flex items-center justify-center overflow-hidden">
          {/* Background grid pattern */}
          <div className="absolute inset-0 opacity-20">
            <svg width="100%" height="100%">
              <defs>
                <pattern id="grid" width="20" height="20" patternUnits="userSpaceOnUse">
                  <path d="M 20 0 L 0 0 0 20" fill="none" stroke="currentColor" strokeWidth="0.5"/>
                </pattern>
              </defs>
              <rect width="100%" height="100%" fill="url(#grid)" />
            </svg>
          </div>

          <div className="absolute inset-0 p-8">
            {/* Connection lines */}
            <svg className="absolute inset-0 w-full h-full">
              {Array.from(agents.values()).slice(0, 6).map((agent, index) => {
                const angle = (index * 60) * Math.PI / 180
                const radius = 120
                const x = 50 + radius * Math.cos(angle) * 0.4
                const y = 50 + radius * Math.sin(angle) * 0.4
                
                return (
                  <line
                    key={`connection-${agent.id}`}
                    x1="50%"
                    y1="50%"
                    x2={`${x}%`}
                    y2={`${y}%`}
                    stroke="currentColor"
                    strokeWidth="2"
                    strokeDasharray={animate ? "5,5" : "none"}
                    className={cn(
                      "opacity-30 transition-all duration-1000",
                      animate && "opacity-60",
                      selectedAgent === agent.id && "opacity-80 stroke-blue-500"
                    )}
                  />
                )
              })}
            </svg>

            {/* Hub node */}
            <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2">
              <div className={cn(
                "w-16 h-16 bg-primary rounded-full flex items-center justify-center text-primary-foreground font-medium cursor-pointer transition-all",
                "hover:scale-110 hover:shadow-lg",
                animate && "animate-pulse"
              )}>
                <Activity className="h-6 w-6" />
              </div>
              <div className="absolute -bottom-6 left-1/2 -translate-x-1/2 text-xs font-medium">
                System Hub
              </div>
            </div>
            
            {/* Agent nodes */}
            {Array.from(agents.values()).slice(0, 8).map((agent, index) => {
              const angle = (index * 45) * Math.PI / 180
              const radius = 120
              const x = 50 + radius * Math.cos(angle) * 0.45
              const y = 50 + radius * Math.sin(angle) * 0.45
              
              const isSelected = selectedAgent === agent.id
              const isActive = agent.status === 'active'
              
              return (
                <div
                  key={agent.id}
                  className="absolute cursor-pointer group"
                  style={{
                    top: `${y}%`,
                    left: `${x}%`,
                    transform: 'translate(-50%, -50%)'
                  }}
                  onClick={() => handleAgentClick(agent.id)}
                >
                  <div className={cn(
                    "w-12 h-12 rounded-full flex items-center justify-center text-xs font-medium transition-all",
                    "hover:scale-110 hover:shadow-lg border-2",
                    isSelected && "ring-2 ring-blue-500 scale-110",
                    isActive ? "bg-green-500 text-white border-green-400" : "bg-gray-500 text-white border-gray-400",
                    agent.type === 'ai' && "bg-blue-500 border-blue-400"
                  )}>
                    {agent.type === 'ai' ? <Bot className="h-4 w-4" /> : <Zap className="h-4 w-4" />}
                  </div>
                  
                  {/* Agent info tooltip */}
                  <div className={cn(
                    "absolute top-full mt-2 p-2 bg-background border rounded-md shadow-lg min-w-max z-10",
                    "opacity-0 group-hover:opacity-100 transition-opacity left-1/2 -translate-x-1/2"
                  )}>
                    <div className="text-xs font-medium">{agent.name || agent.type}</div>
                    <div className="text-xs text-muted-foreground">{String(agent.id).substring(0, 8)}</div>
                    <div className="flex items-center gap-1 mt-1">
                      <Badge variant={isActive ? "default" : "secondary"} className="text-xs">
                        {agent.status || 'unknown'}
                      </Badge>
                      {agent.model && (
                        <Badge variant="outline" className="text-xs">
                          {agent.model}
                        </Badge>
                      )}
                    </div>
                  </div>
                  
                  {/* Status indicator */}
                  <div className={cn(
                    "absolute -top-1 -right-1 w-3 h-3 rounded-full border-2 border-background",
                    isActive ? "bg-green-400" : "bg-gray-400"
                  )} />
                </div>
              )
            })}
          </div>
          
          {agents.size === 0 && (
            <div className="text-center">
              <Bot className="h-12 w-12 mx-auto text-muted-foreground mb-2" />
              <p className="text-muted-foreground">
                No agents to display in network topology
              </p>
              <p className="text-sm text-muted-foreground mt-1">
                Create some agents to see them appear here
              </p>
            </div>
          )}
        </div>
        
        <div className="mt-4 flex gap-4">
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-primary rounded-full" />
            <span className="text-sm">Hub</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-blue-500 rounded-full" />
            <span className="text-sm">AI Agent</span>
          </div>
          <div className="flex items-center gap-2">
            <div className="w-4 h-4 bg-green-500 rounded-full" />
            <span className="text-sm">Simple Agent</span>
          </div>
        </div>
      </CardContent>
    </Card>
  )
}