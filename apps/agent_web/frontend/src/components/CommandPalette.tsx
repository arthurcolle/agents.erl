import { useEffect, useState } from 'react'
import { Dialog, DialogContent } from '@/components/ui/dialog'
import { Command, CommandEmpty, CommandGroup, CommandInput, CommandItem, CommandList, CommandSeparator } from '@/components/ui/command'
import { 
  Bot, 
  Settings, 
  MessageSquare, 
  Terminal, 
  Search,
  Zap,
  Play,
  Square,
  RotateCcw,
  FileText,
  BarChart3
} from 'lucide-react'
import { Badge } from '@/components/ui/badge'

interface CommandPaletteProps {
  isOpen: boolean
  onClose: () => void
  agents: Map<string, any>
  onNavigate: (tab: string) => void
  onSelectAgent: (agentId: string) => void
  onExecuteAction: (action: string, params?: any) => void
}

export function CommandPalette({
  isOpen,
  onClose,
  agents,
  onNavigate,
  onSelectAgent,
  onExecuteAction
}: CommandPaletteProps) {
  const [search, setSearch] = useState('')

  useEffect(() => {
    const down = (e: KeyboardEvent) => {
      if (e.key === 'k' && (e.metaKey || e.ctrlKey)) {
        e.preventDefault()
        onClose()
      }
    }

    document.addEventListener('keydown', down)
    return () => document.removeEventListener('keydown', down)
  }, [onClose])

  const agentList = Array.from(agents.values())

  const pages = [
    { id: 'dashboard', name: 'Dashboard', icon: BarChart3, description: 'System overview and metrics' },
    { id: 'agents', name: 'Agents', icon: Bot, description: 'Manage agents' },
    { id: 'chat', name: 'Chat', icon: MessageSquare, description: 'AI conversations' },
    { id: 'timeline', name: 'Timeline', icon: FileText, description: 'Activity timeline' },
    { id: 'monitoring', name: 'Monitor', icon: BarChart3, description: 'System monitoring' },
    { id: 'settings', name: 'Settings', icon: Settings, description: 'Configuration' }
  ]

  const actions = [
    { id: 'create-agent', name: 'Create Agent', icon: Bot, description: 'Create a new agent instance' },
    { id: 'restart-system', name: 'Restart System', icon: RotateCcw, description: 'Restart the agent system' },
    { id: 'tail-logs', name: 'Tail System Logs', icon: Terminal, description: 'View live system logs' },
    { id: 'clear-logs', name: 'Clear Logs', icon: Square, description: 'Clear all log entries' }
  ]

  const filteredPages = pages.filter(page =>
    page.name.toLowerCase().includes(search.toLowerCase()) ||
    page.description.toLowerCase().includes(search.toLowerCase())
  )

  const filteredAgents = agentList.filter(agent =>
    (agent.name || agent.type).toLowerCase().includes(search.toLowerCase()) ||
    agent.id.toString().includes(search.toLowerCase())
  )

  const filteredActions = actions.filter(action =>
    action.name.toLowerCase().includes(search.toLowerCase()) ||
    action.description.toLowerCase().includes(search.toLowerCase())
  )

  const handleSelect = (value: string) => {
    const [type, id] = value.split(':')
    
    switch (type) {
      case 'page':
        onNavigate(id)
        break
      case 'agent':
        onSelectAgent(id)
        onNavigate('chat')
        break
      case 'action':
        onExecuteAction(id)
        break
    }
    
    onClose()
    setSearch('')
  }

  return (
    <Dialog open={isOpen} onOpenChange={onClose}>
      <DialogContent className="p-0 max-w-2xl">
        <Command className="rounded-lg border-0 shadow-md">
          <div className="flex items-center border-b px-3">
            <Search className="mr-2 h-4 w-4 shrink-0 opacity-50" />
            <CommandInput
              placeholder="Search agents, pages, or actions..."
              value={search}
              onValueChange={setSearch}
              className="border-0 focus:ring-0"
            />
            <Badge variant="outline" className="ml-2 text-xs">
              ⌘K
            </Badge>
          </div>
          
          <CommandList className="max-h-96 overflow-y-auto">
            <CommandEmpty>No results found.</CommandEmpty>
            
            {filteredPages.length > 0 && (
              <CommandGroup heading="Pages">
                {filteredPages.map((page) => (
                  <CommandItem
                    key={page.id}
                    value={`page:${page.id}`}
                    onSelect={handleSelect}
                    className="flex items-center gap-2 px-4 py-2"
                  >
                    <page.icon className="h-4 w-4" />
                    <div className="flex-1">
                      <div className="font-medium">{page.name}</div>
                      <div className="text-xs text-muted-foreground">{page.description}</div>
                    </div>
                  </CommandItem>
                ))}
              </CommandGroup>
            )}

            {filteredAgents.length > 0 && (
              <>
                <CommandSeparator />
                <CommandGroup heading="Agents">
                  {filteredAgents.slice(0, 8).map((agent) => (
                    <CommandItem
                      key={agent.id}
                      value={`agent:${agent.id}`}
                      onSelect={handleSelect}
                      className="flex items-center gap-2 px-4 py-2"
                    >
                      <Bot className="h-4 w-4" />
                      <div className="flex-1">
                        <div className="font-medium">{agent.name || agent.type}</div>
                        <div className="text-xs text-muted-foreground">
                          ID: {String(agent.id).substring(0, 8)} • Status: {agent.status || 'unknown'}
                        </div>
                      </div>
                      <div className="flex gap-1">
                        {agent.status === 'active' && (
                          <div className="h-2 w-2 rounded-full bg-green-500" />
                        )}
                        {agent.status === 'error' && (
                          <div className="h-2 w-2 rounded-full bg-red-500" />
                        )}
                        {agent.model && (
                          <Badge variant="secondary" className="text-xs">
                            {agent.model}
                          </Badge>
                        )}
                      </div>
                    </CommandItem>
                  ))}
                  {filteredAgents.length > 8 && (
                    <CommandItem disabled className="text-xs text-muted-foreground text-center">
                      ... and {filteredAgents.length - 8} more agents
                    </CommandItem>
                  )}
                </CommandGroup>
              </>
            )}

            {filteredActions.length > 0 && (
              <>
                <CommandSeparator />
                <CommandGroup heading="Actions">
                  {filteredActions.map((action) => (
                    <CommandItem
                      key={action.id}
                      value={`action:${action.id}`}
                      onSelect={handleSelect}
                      className="flex items-center gap-2 px-4 py-2"
                    >
                      <action.icon className="h-4 w-4" />
                      <div className="flex-1">
                        <div className="font-medium">{action.name}</div>
                        <div className="text-xs text-muted-foreground">{action.description}</div>
                      </div>
                      <Zap className="h-3 w-3 text-muted-foreground" />
                    </CommandItem>
                  ))}
                </CommandGroup>
              </>
            )}
          </CommandList>
        </Command>
      </DialogContent>
    </Dialog>
  )
}