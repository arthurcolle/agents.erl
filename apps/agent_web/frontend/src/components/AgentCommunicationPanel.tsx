import { useState, useEffect } from 'react'
import { Card } from '@/components/ui/card'
import { Badge } from '@/components/ui/badge'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { ScrollArea } from '@/components/ui/scroll-area'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { 
  ArrowRight, MessageSquare, Users, Activity, Send, 
  Filter, Bot, Clock, Hash, Zap, GitBranch
} from 'lucide-react'

interface Message {
  id: string
  from: string
  to: string
  type: 'direct' | 'broadcast' | 'collaboration'
  content: any
  timestamp: number
  status: 'sent' | 'received' | 'processing'
  tools?: string[]
  collaborationId?: string
}

interface AgentCommunicationPanelProps {
  agents: Map<string, any>
  ws: WebSocket | null
}

export default function AgentCommunicationPanel({ agents, ws }: AgentCommunicationPanelProps) {
  const [messages, setMessages] = useState<Message[]>([])
  const [selectedFrom, setSelectedFrom] = useState<string>('all')
  const [selectedTo, setSelectedTo] = useState<string>('all')
  const [messageType, setMessageType] = useState<'all' | 'direct' | 'broadcast' | 'collaboration'>('all')
  const [newMessage, setNewMessage] = useState('')
  const [sendFrom, setSendFrom] = useState<string>('')
  const [sendTo, setSendTo] = useState<string>('')
  const [collaborations, setCollaborations] = useState<Map<string, any>>(new Map())

  useEffect(() => {
    const handleAgentCommunication = (event: any) => {
      const data = event.detail
      if (data.type === 'agent_communication') {
        const message: Message = {
          id: `msg_${Date.now()}_${Math.random()}`,
          from: data.from,
          to: data.to,
          type: data.messageType || 'direct',
          content: data.content,
          timestamp: Date.now(),
          status: 'received',
          tools: data.tools,
          collaborationId: data.collaborationId
        }
        setMessages(prev => [message, ...prev].slice(0, 1000)) // Keep last 1000 messages
      }
    }

    window.addEventListener('agent_stream', handleAgentCommunication)
    return () => window.removeEventListener('agent_stream', handleAgentCommunication)
  }, [])

  const sendAgentMessage = async () => {
    if (!newMessage || !sendFrom || !sendTo) return

    try {
      const response = await fetch('/api/agents/communicate', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          from: sendFrom,
          to: sendTo,
          message: newMessage,
          type: sendTo === 'broadcast' ? 'broadcast' : 'direct'
        })
      })

      if (response.ok) {
        const sentMessage: Message = {
          id: `msg_${Date.now()}_${Math.random()}`,
          from: sendFrom,
          to: sendTo,
          type: sendTo === 'broadcast' ? 'broadcast' : 'direct',
          content: newMessage,
          timestamp: Date.now(),
          status: 'sent'
        }
        setMessages(prev => [sentMessage, ...prev])
        setNewMessage('')
      }
    } catch (error) {
      console.error('Failed to send message:', error)
    }
  }

  const filteredMessages = messages.filter(msg => {
    if (selectedFrom !== 'all' && msg.from !== selectedFrom) return false
    if (selectedTo !== 'all' && msg.to !== selectedTo && msg.type !== 'broadcast') return false
    if (messageType !== 'all' && msg.type !== messageType) return false
    return true
  })

  const getAgentName = (id: string) => {
    if (id === 'broadcast') return 'All Agents'
    const agent = agents.get(id)
    return agent?.name || agent?.type || id.substring(0, 8)
  }

  const getMessageStats = () => {
    const stats = {
      total: messages.length,
      direct: messages.filter(m => m.type === 'direct').length,
      broadcast: messages.filter(m => m.type === 'broadcast').length,
      collaboration: messages.filter(m => m.type === 'collaboration').length,
      lastMinute: messages.filter(m => Date.now() - m.timestamp < 60000).length
    }
    return stats
  }

  const stats = getMessageStats()

  return (
    <div className="h-full flex flex-col space-y-2">
      {/* Stats Bar */}
      <div className="flex items-center gap-2 text-xs">
        <Badge variant="outline" className="py-0 px-1">
          <Hash className="h-3 w-3 mr-1" />
          {stats.total} total
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <ArrowRight className="h-3 w-3 mr-1" />
          {stats.direct} direct
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <Users className="h-3 w-3 mr-1" />
          {stats.broadcast} broadcast
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <GitBranch className="h-3 w-3 mr-1" />
          {stats.collaboration} collab
        </Badge>
        <Badge variant="outline" className="py-0 px-1">
          <Activity className="h-3 w-3 mr-1" />
          {stats.lastMinute}/min
        </Badge>
      </div>

      {/* Filters */}
      <div className="flex gap-1">
        <Select value={selectedFrom} onValueChange={setSelectedFrom}>
          <SelectTrigger className="h-7 text-xs flex-1">
            <SelectValue placeholder="From" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Agents</SelectItem>
            {Array.from(agents.values()).map(agent => (
              <SelectItem key={agent.id} value={agent.id}>
                {agent.name || agent.type}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        <Select value={selectedTo} onValueChange={setSelectedTo}>
          <SelectTrigger className="h-7 text-xs flex-1">
            <SelectValue placeholder="To" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Recipients</SelectItem>
            <SelectItem value="broadcast">Broadcast</SelectItem>
            {Array.from(agents.values()).map(agent => (
              <SelectItem key={agent.id} value={agent.id}>
                {agent.name || agent.type}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        <Select value={messageType} onValueChange={setMessageType as any}>
          <SelectTrigger className="h-7 text-xs flex-1">
            <SelectValue placeholder="Type" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="all">All Types</SelectItem>
            <SelectItem value="direct">Direct</SelectItem>
            <SelectItem value="broadcast">Broadcast</SelectItem>
            <SelectItem value="collaboration">Collaboration</SelectItem>
          </SelectContent>
        </Select>
      </div>

      {/* Message List */}
      <ScrollArea className="flex-1 border border-gray-800 rounded">
        <div className="p-2 space-y-1">
          {filteredMessages.length === 0 ? (
            <div className="text-center text-xs text-gray-500 py-4">
              No messages yet
            </div>
          ) : (
            filteredMessages.map(message => (
              <div
                key={message.id}
                className={`flex items-start gap-2 p-1 rounded text-xs ${
                  message.status === 'sent' ? 'bg-blue-900/20' : 'bg-gray-900/50'
                } hover:bg-gray-800 transition-colors`}
              >
                <Bot className="h-3 w-3 text-gray-500 mt-0.5 flex-shrink-0" />
                <div className="flex-1 min-w-0">
                  <div className="flex items-center gap-2">
                    <span className="font-medium text-blue-400 truncate">
                      {getAgentName(message.from)}
                    </span>
                    <ArrowRight className="h-3 w-3 text-gray-600" />
                    <span className="font-medium text-green-400 truncate">
                      {getAgentName(message.to)}
                    </span>
                    {message.type !== 'direct' && (
                      <Badge variant="outline" className="py-0 px-1 text-xs">
                        {message.type}
                      </Badge>
                    )}
                    {message.tools && message.tools.length > 0 && (
                      <div className="flex items-center gap-1">
                        <Zap className="h-3 w-3 text-yellow-500" />
                        <span className="text-xs text-gray-500">
                          {message.tools.length} tools
                        </span>
                      </div>
                    )}
                  </div>
                  <div className="text-gray-300 break-all">
                    {typeof message.content === 'string' 
                      ? message.content 
                      : JSON.stringify(message.content, null, 2)}
                  </div>
                  <div className="flex items-center gap-2 mt-0.5 text-gray-500">
                    <Clock className="h-3 w-3" />
                    <span>{new Date(message.timestamp).toLocaleTimeString()}</span>
                    {message.collaborationId && (
                      <>
                        <GitBranch className="h-3 w-3" />
                        <span>Collab: {message.collaborationId.substring(0, 8)}</span>
                      </>
                    )}
                  </div>
                </div>
              </div>
            ))
          )}
        </div>
      </ScrollArea>

      {/* Send Message */}
      <div className="flex gap-1">
        <Select value={sendFrom} onValueChange={setSendFrom}>
          <SelectTrigger className="h-7 text-xs w-24">
            <SelectValue placeholder="From" />
          </SelectTrigger>
          <SelectContent>
            {Array.from(agents.values()).map(agent => (
              <SelectItem key={agent.id} value={agent.id}>
                {agent.name || agent.type}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        <Select value={sendTo} onValueChange={setSendTo}>
          <SelectTrigger className="h-7 text-xs w-24">
            <SelectValue placeholder="To" />
          </SelectTrigger>
          <SelectContent>
            <SelectItem value="broadcast">Broadcast</SelectItem>
            {Array.from(agents.values()).map(agent => (
              <SelectItem key={agent.id} value={agent.id}>
                {agent.name || agent.type}
              </SelectItem>
            ))}
          </SelectContent>
        </Select>

        <Input
          value={newMessage}
          onChange={(e) => setNewMessage(e.target.value)}
          onKeyPress={(e) => e.key === 'Enter' && sendAgentMessage()}
          placeholder="Message..."
          className="h-7 text-xs flex-1"
        />

        <Button
          size="icon"
          onClick={sendAgentMessage}
          disabled={!newMessage || !sendFrom || !sendTo}
          className="h-7 w-7"
        >
          <Send className="h-3 w-3" />
        </Button>
      </div>
    </div>
  )
}