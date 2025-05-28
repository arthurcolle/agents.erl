import { useState, useEffect, useRef } from 'react'
import { Card, CardContent, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Send } from 'lucide-react'
import { cn } from '@/lib/utils'

interface ChatInterfaceProps {
  selectedAgent: string | null
  agents: Map<string, any>
  ws: WebSocket | null
}

interface Message {
  id: string
  sender: 'user' | 'agent'
  content: string
  timestamp: Date
}

export default function ChatInterface({ selectedAgent, agents, ws }: ChatInterfaceProps) {
  const [messages, setMessages] = useState<Message[]>([])
  const [input, setInput] = useState('')
  const [isStreaming, setIsStreaming] = useState(false)
  const messagesEndRef = useRef<HTMLDivElement>(null)

  useEffect(() => {
    scrollToBottom()
  }, [messages])

  useEffect(() => {
    if (selectedAgent && ws && ws.readyState === WebSocket.OPEN) {
      ws.send(JSON.stringify({
        type: 'create_stream',
        agent_id: selectedAgent
      }))
    }
  }, [selectedAgent, ws])

  const scrollToBottom = () => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }

  const sendMessage = () => {
    if (!input.trim() || !selectedAgent || !ws) return

    const userMessage: Message = {
      id: Date.now().toString(),
      sender: 'user',
      content: input,
      timestamp: new Date()
    }

    setMessages(prev => [...prev, userMessage])
    setInput('')

    const agent = agents.get(selectedAgent)
    if (agent?.type === 'ai') {
      setIsStreaming(true)
      ws.send(JSON.stringify({
        type: 'stream_chat',
        message: input
      }))
    } else {
      // Simple agent response
      fetch(`/api/agents/${selectedAgent}/execute`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ action: 'chat', params: { message: input } })
      })
        .then(res => res.json())
        .then(data => {
          const agentMessage: Message = {
            id: Date.now().toString(),
            sender: 'agent',
            content: data.result,
            timestamp: new Date()
          }
          setMessages(prev => [...prev, agentMessage])
        })
    }
  }

  if (!selectedAgent) {
    return (
      <Card>
        <CardContent className="flex items-center justify-center h-96">
          <p className="text-muted-foreground">
            Please select an AI agent from the sidebar to start chatting.
          </p>
        </CardContent>
      </Card>
    )
  }

  const agent = agents.get(selectedAgent)

  return (
    <Card className="h-[600px] flex flex-col">
      <CardHeader>
        <CardTitle>Chat with {agent?.type || 'Agent'}</CardTitle>
      </CardHeader>
      <CardContent className="flex-1 flex flex-col">
        <div className="flex-1 overflow-y-auto space-y-4 mb-4">
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
                  "max-w-[70%] p-3 rounded-lg",
                  message.sender === 'user'
                    ? "bg-primary text-primary-foreground"
                    : "bg-muted"
                )}
              >
                <p className="text-sm">{message.content}</p>
                <p className="text-xs opacity-70 mt-1">
                  {message.timestamp.toLocaleTimeString()}
                </p>
              </div>
            </div>
          ))}
          <div ref={messagesEndRef} />
        </div>
        
        <div className="flex gap-2">
          <Input
            value={input}
            onChange={(e) => setInput(e.target.value)}
            onKeyPress={(e) => e.key === 'Enter' && sendMessage()}
            placeholder="Type your message..."
            disabled={isStreaming}
          />
          <Button onClick={sendMessage} disabled={isStreaming || !input.trim()}>
            <Send className="h-4 w-4" />
          </Button>
        </div>
      </CardContent>
    </Card>
  )
}