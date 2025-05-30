import SimpleAdvancedChat from './SimpleAdvancedChat'

interface ChatInterfaceProps {
  selectedAgent: string | null
  agents: Map<string, any>
  ws: WebSocket | null
}

export default function ChatInterface(props: ChatInterfaceProps) {
  return <SimpleAdvancedChat {...props} />
}