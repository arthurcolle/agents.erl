import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'

interface MonitoringPanelProps {
  agents: Map<string, any>
  selectedAgent?: string
  systemMetrics: {
    cpuUsage: number
    memoryUsage: number
    processCount: number
  }
}

export default function MonitoringPanel({ agents, selectedAgent, systemMetrics }: MonitoringPanelProps) {
  return (
    <div className="space-y-6">
      <div className="grid gap-4 md:grid-cols-2">
        <Card>
          <CardHeader>
            <CardTitle>System Metrics</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-4">
              <div>
                <div className="flex justify-between mb-1">
                  <span className="text-sm font-medium">CPU Usage</span>
                  <span className="text-sm">{systemMetrics.cpuUsage}%</span>
                </div>
                <div className="w-full bg-secondary rounded-full h-2">
                  <div 
                    className="bg-primary h-2 rounded-full transition-all"
                    style={{ width: `${systemMetrics.cpuUsage}%` }}
                  />
                </div>
              </div>
              
              <div>
                <div className="flex justify-between mb-1">
                  <span className="text-sm font-medium">Memory Usage</span>
                  <span className="text-sm">{systemMetrics.memoryUsage}%</span>
                </div>
                <div className="w-full bg-secondary rounded-full h-2">
                  <div 
                    className="bg-primary h-2 rounded-full transition-all"
                    style={{ width: `${systemMetrics.memoryUsage}%` }}
                  />
                </div>
              </div>
              
              <div className="pt-2 border-t">
                <div className="flex justify-between">
                  <span className="text-sm font-medium">Active Processes</span>
                  <span className="text-sm">{systemMetrics.processCount}</span>
                </div>
              </div>
            </div>
          </CardContent>
        </Card>

        <Card>
          <CardHeader>
            <CardTitle>Agent Statistics</CardTitle>
          </CardHeader>
          <CardContent>
            <div className="space-y-2">
              <div className="flex justify-between">
                <span className="text-sm">Total Agents</span>
                <Badge>{agents.size}</Badge>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">AI Agents</span>
                <Badge variant="secondary">
                  {Array.from(agents.values()).filter(a => a.type === 'ai').length}
                </Badge>
              </div>
              <div className="flex justify-between">
                <span className="text-sm">Simple Agents</span>
                <Badge variant="secondary">
                  {Array.from(agents.values()).filter(a => a.type === 'simple').length}
                </Badge>
              </div>
            </div>
          </CardContent>
        </Card>
      </div>

      <Card>
        <CardHeader>
          <CardTitle>Agent Details</CardTitle>
        </CardHeader>
        <CardContent>
          <div className="space-y-4">
            {Array.from(agents.values()).map(agent => (
              <div key={agent.id} className="border rounded-lg p-4">
                <div className="flex items-center justify-between mb-2">
                  <h4 className="font-medium">{agent.type} Agent</h4>
                  <Badge variant="outline">{String(agent.id).substring(0, 8)}</Badge>
                </div>
                <div className="grid grid-cols-3 gap-4 text-sm">
                  <div>
                    <span className="text-muted-foreground">Status:</span>
                    <Badge className="ml-2" variant={agent.status === 'active' ? 'default' : 'secondary'}>
                      {agent.status || 'Active'}
                    </Badge>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Memory:</span>
                    <span className="ml-2">{agent.memory || 0} MB</span>
                  </div>
                  <div>
                    <span className="text-muted-foreground">Messages:</span>
                    <span className="ml-2">{agent.message_queue_len || 0}</span>
                  </div>
                </div>
                <div className="mt-3 flex gap-2">
                  <Button size="sm" variant="outline" onClick={() => {
                    console.log(`View details for agent ${agent.id}`)
                    // TODO: Implement agent details view
                  }}>View Details</Button>
                  <Button size="sm" variant="outline" onClick={async () => {
                    try {
                      // Stop the agent
                      await fetch(`/api/agents/${agent.id}`, { method: 'DELETE' })
                      // Recreate the agent
                      await fetch('/api/agents', {
                        method: 'POST',
                        headers: { 'Content-Type': 'application/json' },
                        body: JSON.stringify({
                          name: agent.name,
                          type: agent.type,
                          system_prompt: agent.system_prompt || 'You are a helpful AI assistant.'
                        })
                      })
                      // Reload agents
                      window.location.reload()
                    } catch (error) {
                      console.error('Failed to restart agent:', error)
                    }
                  }}>Restart</Button>
                </div>
              </div>
            ))}
          </div>
        </CardContent>
      </Card>
    </div>
  )
}