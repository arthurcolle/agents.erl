import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { 
  Send, 
  Users, 
  Bot, 
  Plus, 
  X, 
  MessageSquare, 
  Zap,
  UserPlus,
  Settings,
  Volume2,
  VolumeX
} from 'lucide-react'
import { cn } from '@/lib/utils'
import SimpleAdvancedChat from './SimpleAdvancedChat'

interface MultiAgentChatProps {
  agents: Map<string, any>
  ws: WebSocket | null
  selectedAgent: string | null
  onAgentSelect: (agentId: string) => void
}

interface ChatSession {
  id: string
  name: string
  agentIds: string[]
  messages: any[]
  lastActivity: Date
  isActive: boolean
}

export default function MultiAgentChat({ 
  agents, 
  ws, 
  selectedAgent, 
  onAgentSelect 
}: MultiAgentChatProps) {
  const [chatSessions, setChatSessions] = useState<ChatSession[]>([])
  const [activeChatSession, setActiveChatSession] = useState<string | null>(null)
  const [multiAgentMode, setMultiAgentMode] = useState(false)
  const [selectedAgentsForGroup, setSelectedAgentsForGroup] = useState<string[]>([])
  const [groupChatName, setGroupChatName] = useState('')
  const [showCreateGroup, setShowCreateGroup] = useState(false)
  const [voiceEnabled, setVoiceEnabled] = useState(false)

  // Initialize with existing single agent chat
  useEffect(() => {
    console.log('MultiAgentChat: selectedAgent changed:', selectedAgent, 'activeChatSession:', activeChatSession, 'showCreateGroup:', showCreateGroup)
    
    // Don't auto-create single agent sessions during group creation
    if (selectedAgent && !activeChatSession && !showCreateGroup) {
      const existingSession = chatSessions.find(session => 
        session.agentIds.length === 1 && session.agentIds[0] === selectedAgent
      )
      
      console.log('MultiAgentChat: existingSession found:', existingSession)
      
      if (existingSession) {
        console.log('MultiAgentChat: setting active session to existing:', existingSession.id)
        setActiveChatSession(existingSession.id)
      } else {
        console.log('MultiAgentChat: creating new session for agent:', selectedAgent)
        createSingleAgentSession(selectedAgent)
      }
    }
  }, [selectedAgent, chatSessions, activeChatSession, showCreateGroup])

  // Create single agent chat session
  const createSingleAgentSession = (agentId: string) => {
    console.log('MultiAgentChat: createSingleAgentSession called for:', agentId)
    const agent = agents.get(agentId)
    
    if (!agent) {
      console.log('MultiAgentChat: agent not found for id:', agentId)
      return
    }

    const newSession: ChatSession = {
      id: `session_${Date.now()}`,
      name: agent.name || agent.type,
      agentIds: [agentId],
      messages: [],
      lastActivity: new Date(),
      isActive: true
    }

    console.log('MultiAgentChat: creating new session:', newSession)
    setChatSessions(prev => [...prev, newSession])
    setActiveChatSession(newSession.id)
  }

  // Create multi-agent group chat
  const createGroupChat = () => {
    // Require at least 2 agents, but allow empty group name (will auto-generate)
    if (selectedAgentsForGroup.length < 2) {
      console.log('Need at least 2 agents for group chat, currently selected:', selectedAgentsForGroup.length)
      return
    }

    // Auto-generate group name if empty
    const finalGroupName = groupChatName.trim() || 
      `Group Chat (${selectedAgentsForGroup.map(id => {
        const agent = agents.get(id)
        return agent?.name || agent?.type || id.substring(0, 8)
      }).join(', ')})`

    const newSession: ChatSession = {
      id: `group_${Date.now()}`,
      name: finalGroupName,
      agentIds: selectedAgentsForGroup,
      messages: [],
      lastActivity: new Date(),
      isActive: true
    }

    console.log('Creating group chat:', newSession)
    setChatSessions(prev => [...prev, newSession])
    setActiveChatSession(newSession.id)
    setShowCreateGroup(false)
    setSelectedAgentsForGroup([])
    setGroupChatName('')
  }

  // Close chat session
  const closeChatSession = (sessionId: string) => {
    setChatSessions(prev => prev.filter(session => session.id !== sessionId))
    if (activeChatSession === sessionId) {
      const remainingSessions = chatSessions.filter(session => session.id !== sessionId)
      setActiveChatSession(remainingSessions.length > 0 ? remainingSessions[0].id : null)
    }
  }

  // Get active session
  const activeSession = chatSessions.find(session => session.id === activeChatSession)

  // Agent selection for group chat
  const toggleAgentSelection = (agentId: string) => {
    setSelectedAgentsForGroup(prev => 
      prev.includes(agentId) 
        ? prev.filter(id => id !== agentId)
        : [...prev, agentId]
    )
  }

  return (
    <div className="h-full flex flex-col">
      {/* Multi-Agent Chat Header */}
      <div className="border-b p-3 bg-gradient-to-r from-blue-50 to-purple-50">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-3">
            <Users className="h-5 w-5 text-blue-600" />
            <h2 className="text-lg font-semibold">Multi-Agent Chat</h2>
            <Badge variant="outline">
              {chatSessions.length} active sessions
            </Badge>
          </div>
          <div className="flex items-center gap-2">
            <Button
              variant={voiceEnabled ? "default" : "outline"}
              size="sm"
              onClick={() => setVoiceEnabled(!voiceEnabled)}
            >
              {voiceEnabled ? <Volume2 className="h-4 w-4" /> : <VolumeX className="h-4 w-4" />}
            </Button>
            <Button
              variant="outline"
              size="sm"
              onClick={() => setShowCreateGroup(true)}
            >
              <UserPlus className="h-4 w-4 mr-1" />
              Group Chat
            </Button>
            <Button
              variant={multiAgentMode ? "default" : "outline"}
              size="sm"
              onClick={() => setMultiAgentMode(!multiAgentMode)}
            >
              <Zap className="h-4 w-4 mr-1" />
              {multiAgentMode ? 'Multi-Agent ON' : 'Multi-Agent OFF'}
            </Button>
          </div>
        </div>
      </div>

      {/* Group Chat Creation Modal */}
      {showCreateGroup && (
        <div className="border-b p-4 bg-yellow-50">
          <div className="space-y-3">
            <div className="flex items-center justify-between">
              <h3 className="font-medium">Create Group Chat</h3>
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setShowCreateGroup(false)}
              >
                <X className="h-4 w-4" />
              </Button>
            </div>
            
            <Input
              placeholder="Group chat name (optional - will auto-generate)"
              value={groupChatName}
              onChange={(e) => setGroupChatName(e.target.value)}
              className="w-full"
            />
            
            <div className="space-y-2">
              <div className="text-sm text-gray-600">
                Select agents for group chat (minimum 2 required):
              </div>
              <div className="grid grid-cols-3 gap-2 max-h-40 overflow-y-auto">
                {Array.from(agents.values()).map(agent => (
                  <Button
                    key={agent.id}
                    variant={selectedAgentsForGroup.includes(agent.id) ? "default" : "outline"}
                    size="sm"
                    className="justify-start"
                    onClick={() => toggleAgentSelection(agent.id)}
                  >
                    <Bot className="h-3 w-3 mr-1" />
                    {agent.name || agent.type}
                  </Button>
                ))}
              </div>
            </div>
            
            <div className="flex flex-col gap-2">
              {selectedAgentsForGroup.length < 2 && (
                <div className="text-sm text-amber-600 bg-amber-50 p-2 rounded">
                  Select at least 2 agents to create a group chat
                </div>
              )}
              <Button
                onClick={createGroupChat}
                disabled={selectedAgentsForGroup.length < 2}
                className="w-full"
                size="lg"
              >
                {selectedAgentsForGroup.length < 2 
                  ? `Create Group (${selectedAgentsForGroup.length}/2 minimum)` 
                  : `Create Group Chat (${selectedAgentsForGroup.length} agents)`
                }
              </Button>
            </div>
          </div>
        </div>
      )}

      {/* Chat Session Tabs */}
      {chatSessions.length > 0 && (
        <div className="border-b">
          <Tabs value={activeChatSession || ''} onValueChange={setActiveChatSession}>
            <TabsList className="w-full justify-start h-auto p-1 bg-transparent">
              {chatSessions.map(session => (
                <TabsTrigger
                  key={session.id}
                  value={session.id}
                  className="relative pr-8 data-[state=active]:bg-white data-[state=active]:shadow-sm"
                >
                  <div className="flex items-center gap-2">
                    {session.agentIds.length === 1 ? (
                      <Bot className="h-3 w-3" />
                    ) : (
                      <Users className="h-3 w-3" />
                    )}
                    <span className="text-sm truncate max-w-24">
                      {session.name}
                    </span>
                    {session.agentIds.length > 1 && (
                      <Badge variant="secondary" className="text-xs">
                        {session.agentIds.length}
                      </Badge>
                    )}
                  </div>
                  <Button
                    variant="ghost"
                    size="sm"
                    className="absolute right-1 top-1/2 -translate-y-1/2 h-4 w-4 p-0 hover:bg-red-100"
                    onClick={(e) => {
                      e.stopPropagation()
                      closeChatSession(session.id)
                    }}
                  >
                    <X className="h-3 w-3" />
                  </Button>
                </TabsTrigger>
              ))}
            </TabsList>
          </Tabs>
        </div>
      )}

      {/* Chat Content */}
      <div className="flex-1 overflow-hidden">
        {activeSession ? (
          <div className="h-full">
            {activeSession.agentIds.length === 1 ? (
              // Single agent chat
              <SimpleAdvancedChat
                selectedAgent={activeSession.agentIds[0]}
                agents={agents}
                ws={ws}
              />
            ) : (
              // Multi-agent group chat
              <GroupChatInterface
                session={activeSession}
                agents={agents}
                ws={ws}
                multiAgentMode={multiAgentMode}
                voiceEnabled={voiceEnabled}
              />
            )}
          </div>
        ) : (
          // No active session - show welcome
          <div className="h-full flex items-center justify-center">
            <div className="text-center space-y-4">
              <Users className="h-16 w-16 text-gray-400 mx-auto" />
              <div>
                <h3 className="text-lg font-semibold text-gray-700">Multi-Agent Chat</h3>
                <p className="text-gray-500 mt-2">
                  Select an agent from the sidebar or create a group chat to get started
                </p>
              </div>
              <div className="flex gap-2 justify-center">
                <Button
                  onClick={() => setShowCreateGroup(true)}
                  className="gap-2"
                >
                  <UserPlus className="h-4 w-4" />
                  Create Group Chat
                </Button>
              </div>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}

// Group Chat Interface Component
function GroupChatInterface({ 
  session, 
  agents, 
  ws, 
  multiAgentMode, 
  voiceEnabled 
}: {
  session: ChatSession
  agents: Map<string, any>
  ws: WebSocket | null
  multiAgentMode: boolean
  voiceEnabled: boolean
}) {
  const [input, setInput] = useState('')
  const [messages, setMessages] = useState<any[]>([])
  const [isStreaming, setIsStreaming] = useState(false)
  const messagesEndRef = useRef<HTMLDivElement>(null)

  // Send message to multiple agents
  const sendToGroup = async () => {
    if (!input.trim()) return

    const userMessage = {
      id: Date.now().toString(),
      sender: 'user',
      content: input,
      timestamp: new Date(),
      sessionId: session.id
    }

    setMessages(prev => [...prev, userMessage])
    const messageContent = input
    setInput('')

    if (multiAgentMode) {
      // Send to all agents in parallel
      session.agentIds.forEach(async (agentId, index) => {
        const agent = agents.get(agentId)
        if (!agent) return

        // Add slight delay for natural conversation flow
        setTimeout(async () => {
          try {
            const response = await fetch(`/api/agents/${agentId}/chat`, {
              method: 'POST',
              headers: { 'Content-Type': 'application/json' },
              body: JSON.stringify({ 
                message: messageContent,
                context: `Group chat: ${session.name}`,
                group_members: session.agentIds.map(id => agents.get(id)?.name).join(', ')
              })
            })

            if (response.ok) {
              const data = await response.json()
              const agentMessage = {
                id: `${Date.now()}_${agentId}`,
                sender: 'agent',
                agentId,
                agentName: agent.name || agent.type,
                content: data.response || data.result || 'Response received',
                timestamp: new Date(),
                sessionId: session.id
              }
              setMessages(prev => [...prev, agentMessage])
            }
          } catch (error) {
            console.error(`Failed to get response from agent ${agentId}:`, error)
          }
        }, index * 1000) // Stagger responses
      })
    } else {
      // Send to primary agent only
      const primaryAgentId = session.agentIds[0]
      const agent = agents.get(primaryAgentId)
      if (agent) {
        try {
          const response = await fetch(`/api/agents/${primaryAgentId}/chat`, {
            method: 'POST',
            headers: { 'Content-Type': 'application/json' },
            body: JSON.stringify({ message: messageContent })
          })

          if (response.ok) {
            const data = await response.json()
            const agentMessage = {
              id: Date.now().toString(),
              sender: 'agent',
              agentId: primaryAgentId,
              agentName: agent.name || agent.type,
              content: data.response || data.result || 'Response received',
              timestamp: new Date(),
              sessionId: session.id
            }
            setMessages(prev => [...prev, agentMessage])
          }
        } catch (error) {
          console.error('Failed to send message:', error)
        }
      }
    }
  }

  // Scroll to bottom
  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages])

  return (
    <div className="h-full flex flex-col">
      {/* Group info header */}
      <div className="p-3 border-b bg-gray-50">
        <div className="flex items-center justify-between">
          <div className="flex items-center gap-2">
            <Users className="h-4 w-4" />
            <span className="font-medium">{session.name}</span>
            <Badge variant="outline">{session.agentIds.length} agents</Badge>
          </div>
          <div className="flex gap-1">
            {session.agentIds.map(agentId => {
              const agent = agents.get(agentId)
              return agent ? (
                <Badge key={agentId} variant="secondary" className="text-xs">
                  {agent.name || agent.type}
                </Badge>
              ) : null
            })}
          </div>
        </div>
      </div>

      {/* Messages */}
      <div className="flex-1 overflow-y-auto p-4 space-y-3">
        {messages.map(message => (
          <div
            key={message.id}
            className={cn(
              "flex",
              message.sender === 'user' ? "justify-end" : "justify-start"
            )}
          >
            <div
              className={cn(
                "max-w-[80%] rounded-lg p-3",
                message.sender === 'user'
                  ? "bg-blue-600 text-white"
                  : "bg-gray-100 text-gray-900"
              )}
            >
              {message.sender === 'agent' && (
                <div className="flex items-center gap-2 mb-1">
                  <Bot className="h-3 w-3" />
                  <span className="text-xs font-medium">{message.agentName}</span>
                </div>
              )}
              <p className="text-sm">{message.content}</p>
              <p className="text-xs opacity-70 mt-1">
                {message.timestamp.toLocaleTimeString()}
              </p>
            </div>
          </div>
        ))}
        <div ref={messagesEndRef} />
      </div>

      {/* Input */}
      <div className="p-4 border-t bg-gray-50">
        <div className="flex gap-2">
          <Input
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && sendToGroup()}
            placeholder={multiAgentMode 
              ? `Message all ${session.agentIds.length} agents...` 
              : "Type your message..."
            }
            className="flex-1"
          />
          <Button onClick={sendToGroup} disabled={!input.trim()}>
            <Send className="h-4 w-4" />
          </Button>
        </div>
      </div>
    </div>
  )
}