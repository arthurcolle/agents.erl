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
        <Button size="sm" data-create-agent>
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
                
                {/* Flagship Models */}
                <SelectItem value="ai">AI Agent (GPT-4o)</SelectItem>
                <SelectItem value="researcher">Research Assistant (GPT-4o)</SelectItem>
                <SelectItem value="coder">Code Assistant (GPT-4o)</SelectItem>
                <SelectItem value="analyst">Data Analyst (GPT-4o)</SelectItem>
                <SelectItem value="orchestrator">Task Orchestrator (GPT-4o)</SelectItem>
                <SelectItem value="translator">Language Translator (GPT-4o)</SelectItem>
                <SelectItem value="teacher">Educational Assistant (GPT-4o)</SelectItem>
                <SelectItem value="debugger">Debug Assistant (GPT-4o)</SelectItem>
                
                {/* Reasoning Models */}
                <SelectItem value="reasoning_agent">Advanced Reasoning Agent (o3)</SelectItem>
                <SelectItem value="problem_solver">Complex Problem Solver (o3)</SelectItem>
                <SelectItem value="strategic_planner">Strategic Planning Agent (o3)</SelectItem>
                <SelectItem value="fast_reasoning">Fast Reasoning Agent (o4-mini)</SelectItem>
                <SelectItem value="logic_analyzer">Logic Analysis Agent (o3-mini)</SelectItem>
                <SelectItem value="decision_maker">Decision Making Agent (o1)</SelectItem>
                <SelectItem value="optimization_agent">Optimization Agent (o1-pro)</SelectItem>
                
                {/* Cost-Optimized Models */}
                <SelectItem value="efficient_helper">Efficient Helper (GPT-4o-mini)</SelectItem>
                <SelectItem value="lightweight_assistant">Lightweight Assistant (GPT-4o-mini)</SelectItem>
                <SelectItem value="monitor">System Monitor (GPT-4o-mini)</SelectItem>
                <SelectItem value="quick_support">Quick Support Agent (GPT-4o-mini)</SelectItem>
                
                {/* Specialized Agents */}
                <SelectItem value="creative_writer">Creative Writing Agent (GPT-4o)</SelectItem>
                <SelectItem value="technical_writer">Technical Documentation Agent (GPT-4o)</SelectItem>
                <SelectItem value="security_analyst">Security Analysis Agent (GPT-4o)</SelectItem>
                <SelectItem value="api_specialist">API Integration Specialist (GPT-4o)</SelectItem>
                <SelectItem value="database_expert">Database Expert (GPT-4o)</SelectItem>
                <SelectItem value="devops_engineer">DevOps Engineer (GPT-4o)</SelectItem>
                <SelectItem value="ui_designer">UI/UX Design Assistant (GPT-4o)</SelectItem>
                <SelectItem value="project_manager">Project Manager (GPT-4o)</SelectItem>
                <SelectItem value="qa_tester">QA Testing Agent (GPT-4o)</SelectItem>
                <SelectItem value="performance_optimizer">Performance Optimizer (o3)</SelectItem>
                
                {/* Industry-Specific Agents */}
                <SelectItem value="finance_advisor">Financial Advisor (GPT-4o)</SelectItem>
                <SelectItem value="legal_assistant">Legal Research Assistant (GPT-4o)</SelectItem>
                <SelectItem value="medical_researcher">Medical Research Agent (GPT-4o)</SelectItem>
                <SelectItem value="marketing_specialist">Marketing Specialist (GPT-4o)</SelectItem>
                <SelectItem value="sales_assistant">Sales Assistant (GPT-4o)</SelectItem>
                <SelectItem value="hr_specialist">HR Specialist (GPT-4o)</SelectItem>
                <SelectItem value="content_moderator">Content Moderator (GPT-4o)</SelectItem>
                
                {/* Legacy Options */}
                <SelectItem value="legacy_assistant">Legacy Assistant (GPT-3.5-turbo)</SelectItem>
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