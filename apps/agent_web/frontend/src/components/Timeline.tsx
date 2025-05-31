import { useState, useEffect } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Badge } from '@/components/ui/badge'
import { Input } from '@/components/ui/input'
import { 
  Clock, 
  User, 
  Bot, 
  MessageSquare, 
  Activity,
  Filter,
  Calendar,
  ChevronDown,
  ChevronRight,
  Save,
  Trash2,
  FileText,
  Image as ImageIcon,
  Code,
  AlertCircle,
  CheckCircle,
  XCircle,
  Zap
} from 'lucide-react'
import { cn } from '@/lib/utils'
import { format, formatDistanceToNow } from 'date-fns'

interface TimelineEvent {
  id: string
  timestamp: Date
  type: 'message' | 'system' | 'agent_action' | 'error' | 'warning' | 'success'
  source: 'user' | 'agent' | 'system'
  agentId?: string
  agentName?: string
  conversationId?: string
  content: string
  metadata?: {
    model?: string
    tokens?: number
    duration?: number
    format?: string
    images?: string[]
    attachments?: Array<{
      name: string
      url: string
      type: string
    }>
  }
}

interface TimelineProps {
  ws: WebSocket | null
  agents: Map<string, any>
}

export default function Timeline({ ws, agents }: TimelineProps) {
  const [events, setEvents] = useState<TimelineEvent[]>([])
  const [filteredEvents, setFilteredEvents] = useState<TimelineEvent[]>([])
  const [filter, setFilter] = useState({
    type: 'all',
    source: 'all',
    agentId: 'all',
    searchTerm: ''
  })
  const [groupBy, setGroupBy] = useState<'none' | 'conversation' | 'agent' | 'time'>('time')
  const [expandedGroups, setExpandedGroups] = useState<Set<string>>(new Set())
  const [autoUpdate, setAutoUpdate] = useState(true)

  // Load persisted events on mount
  useEffect(() => {
    loadTimelineEvents()
    
    // Set up WebSocket listeners
    if (ws) {
      const handleMessage = (event: MessageEvent) => {
        try {
          const data = JSON.parse(event.data)
          handleWebSocketEvent(data)
        } catch (error) {
          console.error('Failed to parse WebSocket message:', error)
        }
      }
      
      ws.addEventListener('message', handleMessage)
      return () => ws.removeEventListener('message', handleMessage)
    }
  }, [ws])

  // Load events from backend
  const loadTimelineEvents = async () => {
    try {
      const response = await fetch('/api/timeline/events')
      if (response.ok) {
        const data = await response.json()
        const parsedEvents = data.events.map((event: any) => ({
          ...event,
          timestamp: new Date(event.timestamp)
        }))
        setEvents(parsedEvents)
      }
    } catch (error) {
      console.error('Failed to load timeline events:', error)
    }
  }

  // Handle WebSocket events
  const handleWebSocketEvent = (data: any) => {
    if (!autoUpdate) return

    let newEvent: TimelineEvent | null = null

    switch (data.type) {
      case 'stream_token':
      case 'stream_complete':
        // Handle streaming messages
        newEvent = {
          id: `event_${Date.now()}_${Math.random()}`,
          timestamp: new Date(),
          type: 'message',
          source: 'agent',
          agentId: data.agent_id,
          agentName: agents.get(data.agent_id)?.name || 'Unknown Agent',
          conversationId: data.conversation_id,
          content: data.token || data.result || '',
          metadata: {
            model: data.model,
            tokens: data.tokens
          }
        }
        break

      case 'agent_event':
        newEvent = {
          id: `event_${Date.now()}_${Math.random()}`,
          timestamp: new Date(),
          type: 'agent_action',
          source: 'agent',
          agentId: data.agent_id,
          agentName: agents.get(data.agent_id)?.name || 'Unknown Agent',
          content: data.event.description || 'Agent action',
          metadata: data.event.metadata
        }
        break

      case 'system_metrics':
        newEvent = {
          id: `event_${Date.now()}_${Math.random()}`,
          timestamp: new Date(),
          type: 'system',
          source: 'system',
          content: `System metrics update: CPU ${data.data.cpuUsage}%, Memory ${data.data.memoryUsage}%`,
          metadata: data.data
        }
        break

      case 'error':
        newEvent = {
          id: `event_${Date.now()}_${Math.random()}`,
          timestamp: new Date(),
          type: 'error',
          source: 'system',
          content: data.message || 'System error occurred',
          metadata: data.details
        }
        break
    }

    if (newEvent) {
      addEvent(newEvent)
    }
  }

  // Add event and persist
  const addEvent = async (event: TimelineEvent) => {
    setEvents(prev => [event, ...prev])
    
    // Persist to backend
    try {
      await fetch('/api/timeline/events', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(event)
      })
    } catch (error) {
      console.error('Failed to persist timeline event:', error)
    }
  }

  // Apply filters
  useEffect(() => {
    let filtered = events

    if (filter.type !== 'all') {
      filtered = filtered.filter(event => event.type === filter.type)
    }

    if (filter.source !== 'all') {
      filtered = filtered.filter(event => event.source === filter.source)
    }

    if (filter.agentId !== 'all') {
      filtered = filtered.filter(event => event.agentId === filter.agentId)
    }

    if (filter.searchTerm) {
      const searchLower = filter.searchTerm.toLowerCase()
      filtered = filtered.filter(event => 
        event.content.toLowerCase().includes(searchLower) ||
        event.agentName?.toLowerCase().includes(searchLower)
      )
    }

    setFilteredEvents(filtered)
  }, [events, filter])

  // Group events
  const groupEvents = () => {
    if (groupBy === 'none') {
      return [{ key: 'all', label: 'All Events', events: filteredEvents }]
    }

    const groups = new Map<string, TimelineEvent[]>()

    filteredEvents.forEach(event => {
      let key = ''
      let label = ''

      switch (groupBy) {
        case 'conversation':
          key = event.conversationId || 'no-conversation'
          label = event.conversationId ? `Conversation ${event.conversationId.substring(0, 8)}` : 'No Conversation'
          break
        case 'agent':
          key = event.agentId || 'system'
          label = event.agentName || 'System'
          break
        case 'time':
          const dateKey = format(event.timestamp, 'yyyy-MM-dd')
          key = dateKey
          label = format(event.timestamp, 'MMMM d, yyyy')
          break
      }

      if (!groups.has(key)) {
        groups.set(key, [])
      }
      groups.get(key)!.push(event)
    })

    return Array.from(groups.entries()).map(([key, events]) => ({
      key,
      label: key,
      events
    }))
  }

  // Toggle group expansion
  const toggleGroup = (groupKey: string) => {
    setExpandedGroups(prev => {
      const next = new Set(prev)
      if (next.has(groupKey)) {
        next.delete(groupKey)
      } else {
        next.add(groupKey)
      }
      return next
    })
  }

  // Clear timeline
  const clearTimeline = async () => {
    if (confirm('Are you sure you want to clear the timeline? This action cannot be undone.')) {
      setEvents([])
      try {
        await fetch('/api/timeline/events', { method: 'DELETE' })
      } catch (error) {
        console.error('Failed to clear timeline:', error)
      }
    }
  }

  // Export timeline
  const exportTimeline = () => {
    const dataStr = JSON.stringify(events, null, 2)
    const dataUri = 'data:application/json;charset=utf-8,'+ encodeURIComponent(dataStr)
    
    const exportFileDefaultName = `timeline_${format(new Date(), 'yyyy-MM-dd_HH-mm-ss')}.json`
    
    const linkElement = document.createElement('a')
    linkElement.setAttribute('href', dataUri)
    linkElement.setAttribute('download', exportFileDefaultName)
    linkElement.click()
  }

  // Get event icon
  const getEventIcon = (event: TimelineEvent) => {
    switch (event.type) {
      case 'message':
        return <MessageSquare className="h-4 w-4" />
      case 'system':
        return <Activity className="h-4 w-4" />
      case 'agent_action':
        return <Zap className="h-4 w-4" />
      case 'error':
        return <XCircle className="h-4 w-4 text-red-500" />
      case 'warning':
        return <AlertCircle className="h-4 w-4 text-yellow-500" />
      case 'success':
        return <CheckCircle className="h-4 w-4 text-green-500" />
      default:
        return <Activity className="h-4 w-4" />
    }
  }

  // Get event color
  const getEventColor = (event: TimelineEvent) => {
    switch (event.type) {
      case 'error':
        return 'border-red-500 bg-red-50'
      case 'warning':
        return 'border-yellow-500 bg-yellow-50'
      case 'success':
        return 'border-green-500 bg-green-50'
      case 'agent_action':
        return 'border-blue-500 bg-blue-50'
      default:
        return 'border-gray-300'
    }
  }

  const eventGroups = groupEvents()

  return (
    <Card className="h-full flex flex-col">
      <CardHeader>
        <div className="flex items-center justify-between">
          <CardTitle className="flex items-center gap-2">
            <Clock className="h-5 w-5" />
            Timeline
            <Badge variant="secondary">{events.length} events</Badge>
          </CardTitle>
          <div className="flex items-center gap-2">
            <Button
              variant={autoUpdate ? "default" : "outline"}
              size="sm"
              onClick={() => setAutoUpdate(!autoUpdate)}
            >
              {autoUpdate ? 'Auto-update ON' : 'Auto-update OFF'}
            </Button>
            <Button variant="outline" size="sm" onClick={exportTimeline}>
              <Save className="h-4 w-4 mr-1" />
              Export
            </Button>
            <Button variant="outline" size="sm" onClick={clearTimeline}>
              <Trash2 className="h-4 w-4 mr-1" />
              Clear
            </Button>
          </div>
        </div>
      </CardHeader>
      <CardContent className="flex-1 flex flex-col overflow-hidden">
        {/* Filters */}
        <div className="mb-4 space-y-2">
          <div className="flex gap-2">
            <Input
              placeholder="Search events..."
              value={filter.searchTerm}
              onChange={(e) => setFilter({ ...filter, searchTerm: e.target.value })}
              className="flex-1"
            />
            <select
              value={filter.type}
              onChange={(e) => setFilter({ ...filter, type: e.target.value })}
              className="px-3 py-2 border rounded-md"
            >
              <option value="all">All Types</option>
              <option value="message">Messages</option>
              <option value="system">System</option>
              <option value="agent_action">Agent Actions</option>
              <option value="error">Errors</option>
              <option value="warning">Warnings</option>
              <option value="success">Success</option>
            </select>
            <select
              value={filter.source}
              onChange={(e) => setFilter({ ...filter, source: e.target.value })}
              className="px-3 py-2 border rounded-md"
            >
              <option value="all">All Sources</option>
              <option value="user">User</option>
              <option value="agent">Agent</option>
              <option value="system">System</option>
            </select>
            <select
              value={filter.agentId}
              onChange={(e) => setFilter({ ...filter, agentId: e.target.value })}
              className="px-3 py-2 border rounded-md"
            >
              <option value="all">All Agents</option>
              {Array.from(agents.entries()).map(([id, agent]) => (
                <option key={id} value={id}>{agent.name || agent.type}</option>
              ))}
            </select>
            <select
              value={groupBy}
              onChange={(e) => setGroupBy(e.target.value as any)}
              className="px-3 py-2 border rounded-md"
            >
              <option value="none">No Grouping</option>
              <option value="time">Group by Time</option>
              <option value="conversation">Group by Conversation</option>
              <option value="agent">Group by Agent</option>
            </select>
          </div>
        </div>

        {/* Timeline */}
        <div className="flex-1 overflow-y-auto space-y-4">
          {eventGroups.map(group => (
            <div key={group.key} className="space-y-2">
              {groupBy !== 'none' && (
                <button
                  onClick={() => toggleGroup(group.key)}
                  className="flex items-center gap-2 text-sm font-medium text-gray-700 hover:text-gray-900"
                >
                  {expandedGroups.has(group.key) ? <ChevronDown className="h-4 w-4" /> : <ChevronRight className="h-4 w-4" />}
                  {group.label}
                  <Badge variant="secondary" className="ml-2">{group.events.length}</Badge>
                </button>
              )}
              
              {(groupBy === 'none' || expandedGroups.has(group.key)) && (
                <div className="space-y-2 ml-6">
                  {group.events.map(event => (
                    <div
                      key={event.id}
                      className={cn(
                        "flex gap-3 p-3 rounded-lg border transition-all hover:shadow-sm",
                        getEventColor(event)
                      )}
                    >
                      <div className="flex-shrink-0 mt-1">
                        {getEventIcon(event)}
                      </div>
                      <div className="flex-1 min-w-0">
                        <div className="flex items-start justify-between gap-2">
                          <div className="flex-1">
                            <div className="flex items-center gap-2 flex-wrap">
                              <span className="text-sm font-medium">
                                {event.source === 'user' ? 'User' : event.agentName || 'System'}
                              </span>
                              {event.type !== 'message' && (
                                <Badge variant="outline" className="text-xs">
                                  {event.type.replace('_', ' ')}
                                </Badge>
                              )}
                              {event.metadata?.model && (
                                <Badge variant="secondary" className="text-xs">
                                  {event.metadata.model}
                                </Badge>
                              )}
                              {event.metadata?.format && (
                                <Badge variant="secondary" className="text-xs">
                                  {event.metadata.format}
                                </Badge>
                              )}
                            </div>
                            <p className="text-sm text-gray-700 mt-1 break-words">
                              {event.content.length > 200 
                                ? event.content.substring(0, 200) + '...' 
                                : event.content}
                            </p>
                            {event.metadata?.images && event.metadata.images.length > 0 && (
                              <div className="flex items-center gap-1 mt-2">
                                <ImageIcon className="h-3 w-3 text-gray-500" />
                                <span className="text-xs text-gray-500">
                                  {event.metadata.images.length} image{event.metadata.images.length > 1 ? 's' : ''}
                                </span>
                              </div>
                            )}
                            {event.metadata?.duration && (
                              <span className="text-xs text-gray-500">
                                Duration: {event.metadata.duration}ms
                              </span>
                            )}
                          </div>
                          <div className="text-xs text-gray-500 whitespace-nowrap">
                            <div title={format(event.timestamp, 'PPpp')}>
                              {formatDistanceToNow(event.timestamp, { addSuffix: true })}
                            </div>
                          </div>
                        </div>
                      </div>
                    </div>
                  ))}
                </div>
              )}
            </div>
          ))}
          
          {filteredEvents.length === 0 && (
            <div className="text-center text-gray-500 py-8">
              No events match your filters
            </div>
          )}
        </div>
      </CardContent>
    </Card>
  )
}