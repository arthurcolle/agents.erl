import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { RefreshCw, Maximize2 } from 'lucide-react'
import { cn } from '@/lib/utils'

interface NetworkTopologyProps {
  agents: Map<string, any>
}

export default function NetworkTopology({ agents }: NetworkTopologyProps) {
  const refreshTopology = () => {
    console.log('Refreshing network topology...')
  }

  const autoLayout = () => {
    console.log('Auto-layout network nodes...')
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
        <div className="relative h-96 bg-muted rounded-lg flex items-center justify-center">
          <div className="absolute inset-0 p-8">
            {/* Hub node */}
            <div className="absolute top-1/2 left-1/2 -translate-x-1/2 -translate-y-1/2">
              <div className="w-16 h-16 bg-primary rounded-full flex items-center justify-center text-primary-foreground font-medium">
                Hub
              </div>
            </div>
            
            {/* Agent nodes */}
            {Array.from(agents.values()).slice(0, 6).map((agent, index) => {
              const angle = (index * 60) * Math.PI / 180
              const radius = 120
              const x = 50 + radius * Math.cos(angle) * 0.4
              const y = 50 + radius * Math.sin(angle) * 0.4
              
              return (
                <div
                  key={agent.id}
                  className="absolute"
                  style={{
                    top: `${y}%`,
                    left: `${x}%`,
                    transform: 'translate(-50%, -50%)'
                  }}
                >
                  <div className={cn(
                    "w-12 h-12 rounded-full flex items-center justify-center text-xs font-medium",
                    agent.type === 'ai' ? "bg-blue-500 text-white" : "bg-green-500 text-white"
                  )}>
                    {agent.type === 'ai' ? 'AI' : 'S'}
                  </div>
                  <div className="absolute top-full mt-1 text-xs text-center whitespace-nowrap">
                    {String(agent.id).substring(0, 6)}
                  </div>
                </div>
              )
            })}
          </div>
          
          {agents.size === 0 && (
            <p className="text-muted-foreground">
              No agents to display in network topology
            </p>
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