import { Badge } from '@/components/ui/badge'
import { cn } from '@/lib/utils'

interface AgentListProps {
  agents: Map<string, any>
  selectedAgent: string | null
  onSelectAgent: (agentId: string) => void
}

export default function AgentList({ agents, selectedAgent, onSelectAgent }: AgentListProps) {
  if (agents.size === 0) {
    return (
      <div className="text-center py-4 text-muted-foreground">
        No agents running
      </div>
    )
  }

  // Convert to array and sort for stable ordering
  const sortedAgents = Array.from(agents.values()).sort((a, b) => a.id.localeCompare(b.id))

  return (
    <div className="space-y-2">
      {sortedAgents.map(agent => (
        <div
          key={agent.id} // Use agent.id as key, not array index
          className={cn(
            "flex items-center justify-between p-2 rounded-md cursor-pointer transition-colors",
            "hover:bg-accent hover:text-accent-foreground",
            selectedAgent === agent.id && "bg-accent text-accent-foreground"
          )}
          onClick={() => onSelectAgent(agent.id)}
        >
          <div className="flex items-center gap-2">
            <div className={cn(
              "w-2 h-2 rounded-full transition-colors",
              agent.status === 'active' ? "bg-green-500" : "bg-gray-400"
            )} />
            <div className="flex flex-col">
              <span className="font-medium text-sm">{agent.name || agent.type}</span>
              {agent.model && (
                <Badge variant="secondary" className="text-xs w-fit">
                  {agent.model}
                </Badge>
              )}
            </div>
          </div>
          <Badge variant="outline" className="text-xs">
            {String(agent.id).substring(0, 8)}
          </Badge>
        </div>
      ))}
    </div>
  )
}