import React, { useMemo } from 'react'
import { useVirtualizer } from '@tanstack/react-virtual'
import { AgentPillXL } from './AgentPillXL'
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible'
import { Badge } from '@/components/ui/badge'
import { ChevronDown, Zap, AlertTriangle, Moon, Bot } from 'lucide-react'
import { useState } from 'react'

interface VirtualizedAgentListProps {
  agents: Map<string, any>
  selectedAgent: string | null
  onSelectAgent: (id: string) => void
  onChatWithAgent: (id: string) => void
  isCompact?: boolean
}

export function VirtualizedAgentList({
  agents,
  selectedAgent,
  onSelectAgent,
  onChatWithAgent,
  isCompact = false
}: VirtualizedAgentListProps) {
  const [expandedSections, setExpandedSections] = useState({
    hot: true,
    errors: true,
    idle: false,
    all: true
  })

  const agentsByCategory = useMemo(() => {
    // Sort agents by ID for stable ordering
    const agentList = Array.from(agents.values()).sort((a, b) => a.id.localeCompare(b.id))
    
    return {
      hot: agentList.filter(a => a.status === 'active' && (a.lastActivity || 0) > Date.now() - 300000), // Active in last 5 min
      errors: agentList.filter(a => a.status === 'error'),
      idle: agentList.filter(a => a.status === 'idle' || (!a.lastActivity || a.lastActivity < Date.now() - 900000)), // Idle for 15+ min
      all: agentList
    }
  }, [agents])

  const toggleSection = (section: keyof typeof expandedSections) => {
    setExpandedSections(prev => ({
      ...prev,
      [section]: !prev[section]
    }))
  }

  const SectionHeader = ({ 
    title, 
    count, 
    icon: Icon, 
    variant = 'default',
    isExpanded,
    onToggle 
  }: {
    title: string
    count: number
    icon: any
    variant?: 'default' | 'destructive' | 'secondary'
    isExpanded: boolean
    onToggle: () => void
  }) => (
    <CollapsibleTrigger 
      className="flex items-center justify-between w-full p-2 hover:bg-accent/50 rounded-md text-left"
      onClick={onToggle}
    >
      <div className="flex items-center gap-2">
        <Icon className="h-3 w-3" />
        <span className="text-sm font-medium">{title}</span>
        <Badge variant={variant} className="text-xs">
          {count}
        </Badge>
      </div>
      <ChevronDown className={`h-3 w-3 transition-transform ${isExpanded ? 'rotate-180' : ''}`} />
    </CollapsibleTrigger>
  )

  const AgentSection = ({ 
    agents: sectionAgents, 
    isExpanded 
  }: { 
    agents: any[], 
    isExpanded: boolean 
  }) => {
    const parentRef = React.useRef<HTMLDivElement>(null)
    
    const virtualizer = useVirtualizer({
      count: isExpanded ? sectionAgents.length : 0,
      getScrollElement: () => parentRef.current,
      estimateSize: () => isCompact ? 32 : 56,
      overscan: 5,
    })

    if (!isExpanded) return null

    return (
      <div
        ref={parentRef}
        className="max-h-40 overflow-auto"
        style={{
          height: `${virtualizer.getTotalSize()}px`,
          position: 'relative',
        }}
      >
        {virtualizer.getVirtualItems().map((virtualItem) => {
          const agent = sectionAgents[virtualItem.index]
          return (
            <div
              key={virtualItem.key}
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: '100%',
                height: `${virtualItem.size}px`,
                transform: `translateY(${virtualItem.start}px)`,
              }}
            >
              <AgentPillXL
                agent={agent}
                isSelected={selectedAgent === agent.id}
                isCompact={isCompact}
                onClick={() => onSelectAgent(agent.id)}
                onChat={() => onChatWithAgent(agent.id)}
              />
            </div>
          )
        })}
      </div>
    )
  }

  if (isCompact) {
    // In compact mode, show only a simple list
    const parentRef = React.useRef<HTMLDivElement>(null)
    
    const virtualizer = useVirtualizer({
      count: agentsByCategory.all.length,
      getScrollElement: () => parentRef.current,
      estimateSize: () => 32,
      overscan: 5,
    })

    return (
      <div
        ref={parentRef}
        className="h-full overflow-auto"
        style={{
          height: `${virtualizer.getTotalSize()}px`,
          position: 'relative',
        }}
      >
        {virtualizer.getVirtualItems().map((virtualItem) => {
          const agent = agentsByCategory.all[virtualItem.index]
          return (
            <div
              key={virtualItem.key}
              style={{
                position: 'absolute',
                top: 0,
                left: 0,
                width: '100%',
                height: `${virtualItem.size}px`,
                transform: `translateY(${virtualItem.start}px)`,
              }}
            >
              <AgentPillXL
                agent={agent}
                isSelected={selectedAgent === agent.id}
                isCompact={true}
                onClick={() => onSelectAgent(agent.id)}
                onChat={() => onChatWithAgent(agent.id)}
              />
            </div>
          )
        })}
      </div>
    )
  }

  return (
    <div className="space-y-1">
      {/* Hot Agents */}
      <Collapsible open={expandedSections.hot}>
        <SectionHeader
          title="Hot"
          count={agentsByCategory.hot.length}
          icon={Zap}
          variant="default"
          isExpanded={expandedSections.hot}
          onToggle={() => toggleSection('hot')}
        />
        <CollapsibleContent>
          <AgentSection 
            agents={agentsByCategory.hot} 
            isExpanded={expandedSections.hot}
          />
        </CollapsibleContent>
      </Collapsible>

      {/* Error Agents */}
      {agentsByCategory.errors.length > 0 && (
        <Collapsible open={expandedSections.errors}>
          <SectionHeader
            title="Errors"
            count={agentsByCategory.errors.length}
            icon={AlertTriangle}
            variant="destructive"
            isExpanded={expandedSections.errors}
            onToggle={() => toggleSection('errors')}
          />
          <CollapsibleContent>
            <AgentSection 
              agents={agentsByCategory.errors} 
              isExpanded={expandedSections.errors}
            />
          </CollapsibleContent>
        </Collapsible>
      )}

      {/* Idle Agents */}
      {agentsByCategory.idle.length > 0 && (
        <Collapsible open={expandedSections.idle}>
          <SectionHeader
            title="Idle"
            count={agentsByCategory.idle.length}
            icon={Moon}
            variant="secondary"
            isExpanded={expandedSections.idle}
            onToggle={() => toggleSection('idle')}
          />
          <CollapsibleContent>
            <AgentSection 
              agents={agentsByCategory.idle} 
              isExpanded={expandedSections.idle}
            />
          </CollapsibleContent>
        </Collapsible>
      )}

      {/* All Agents */}
      <Collapsible open={expandedSections.all}>
        <SectionHeader
          title="All Agents"
          count={agentsByCategory.all.length}
          icon={Bot}
          variant="secondary"
          isExpanded={expandedSections.all}
          onToggle={() => toggleSection('all')}
        />
        <CollapsibleContent>
          <AgentSection 
            agents={agentsByCategory.all} 
            isExpanded={expandedSections.all}
          />
        </CollapsibleContent>
      </Collapsible>
    </div>
  )
}

