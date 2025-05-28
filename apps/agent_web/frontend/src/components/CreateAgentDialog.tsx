import { useState } from 'react'
import { Dialog, DialogContent, DialogDescription, DialogFooter, DialogHeader, DialogTitle, DialogTrigger } from '@/components/ui/dialog'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Plus } from 'lucide-react'

interface CreateAgentDialogProps {
  onCreateAgent: (type: string, name: string, tools?: string[]) => Promise<void>
}

export default function CreateAgentDialog({ onCreateAgent }: CreateAgentDialogProps) {
  const [open, setOpen] = useState(false)
  const [agentType, setAgentType] = useState('simple')
  const [agentName, setAgentName] = useState('')
  const [selectedTools, setSelectedTools] = useState<string[]>([])
  const [isCreating, setIsCreating] = useState(false)

  const tools = [
    { value: 'shell', label: 'Shell Commands' },
    { value: 'file', label: 'File Operations' },
    { value: 'http', label: 'HTTP Requests' },
    { value: 'data', label: 'Data Processing' }
  ]

  const handleCreate = async () => {
    if (!agentName.trim()) return
    
    setIsCreating(true)
    try {
      await onCreateAgent(agentType, agentName, agentType === 'ai' ? selectedTools : undefined)
      setOpen(false)
      setAgentName('')
      setSelectedTools([])
      setAgentType('simple')
    } catch (error) {
      console.error('Failed to create agent:', error)
    } finally {
      setIsCreating(false)
    }
  }

  return (
    <Dialog open={open} onOpenChange={setOpen}>
      <DialogTrigger asChild>
        <Button size="sm">
          <Plus className="h-4 w-4 mr-1" />
          New Agent
        </Button>
      </DialogTrigger>
      <DialogContent className="sm:max-w-[425px]">
        <DialogHeader>
          <DialogTitle>Create New Agent</DialogTitle>
          <DialogDescription>
            Configure and deploy a new agent to your system.
          </DialogDescription>
        </DialogHeader>
        <div className="grid gap-4 py-4">
          <div className="grid grid-cols-4 items-center gap-4">
            <Label htmlFor="type" className="text-right">
              Type
            </Label>
            <Select value={agentType} onValueChange={setAgentType}>
              <SelectTrigger className="col-span-3">
                <SelectValue />
              </SelectTrigger>
              <SelectContent>
                <SelectItem value="simple">Simple Agent</SelectItem>
                <SelectItem value="ai">AI Agent (GPT-4)</SelectItem>
                <SelectItem value="researcher">Research Assistant (GPT-4)</SelectItem>
                <SelectItem value="coder">Code Assistant (GPT-4)</SelectItem>
                <SelectItem value="analyst">Data Analyst (GPT-4)</SelectItem>
                <SelectItem value="advanced_researcher">Advanced Research Agent (GPT-4.1)</SelectItem>
                <SelectItem value="efficient_coder">Efficient Code Assistant (GPT-4.1-mini)</SelectItem>
                <SelectItem value="lightweight_helper">Lightweight Assistant (GPT-4.1-nano)</SelectItem>
                <SelectItem value="quantum_analyst">Quantum Data Analyst (GPT-4.1)</SelectItem>
                <SelectItem value="micro_orchestrator">Micro Orchestrator (GPT-4.1-nano)</SelectItem>
                <SelectItem value="orchestrator">Task Orchestrator (GPT-4)</SelectItem>
                <SelectItem value="monitor">System Monitor (GPT-3.5-turbo)</SelectItem>
                <SelectItem value="translator">Language Translator (GPT-4)</SelectItem>
                <SelectItem value="teacher">Educational Assistant (GPT-4)</SelectItem>
                <SelectItem value="debugger">Debug Assistant (GPT-4)</SelectItem>
              </SelectContent>
            </Select>
          </div>
          <div className="grid grid-cols-4 items-center gap-4">
            <Label htmlFor="name" className="text-right">
              Name
            </Label>
            <Input
              id="name"
              value={agentName}
              onChange={(e) => setAgentName(e.target.value)}
              placeholder="Enter agent name"
              className="col-span-3"
            />
          </div>
          {agentType === 'ai' && (
            <div className="grid grid-cols-4 items-start gap-4">
              <Label className="text-right pt-3">
                Tools
              </Label>
              <div className="col-span-3 space-y-2">
                {tools.map(tool => (
                  <label key={tool.value} className="flex items-center space-x-2">
                    <input
                      type="checkbox"
                      value={tool.value}
                      checked={selectedTools.includes(tool.value)}
                      onChange={(e) => {
                        if (e.target.checked) {
                          setSelectedTools([...selectedTools, tool.value])
                        } else {
                          setSelectedTools(selectedTools.filter(t => t !== tool.value))
                        }
                      }}
                      className="rounded"
                    />
                    <span className="text-sm">{tool.label}</span>
                  </label>
                ))}
              </div>
            </div>
          )}
        </div>
        <DialogFooter>
          <Button variant="outline" onClick={() => setOpen(false)}>
            Cancel
          </Button>
          <Button onClick={handleCreate} disabled={!agentName.trim() || isCreating}>
            {isCreating ? 'Creating...' : 'Create Agent'}
          </Button>
        </DialogFooter>
      </DialogContent>
    </Dialog>
  )
}