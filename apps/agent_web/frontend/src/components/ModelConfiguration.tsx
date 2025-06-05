import { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { Badge } from '@/components/ui/badge'
import { Switch } from '@/components/ui/switch'
import { Slider } from '@/components/ui/slider'
import { Textarea } from '@/components/ui/textarea'
import { 
  Settings, 
  Brain, 
  Key, 
  Zap, 
  Server, 
  TestTube,
  Save,
  RefreshCw,
  Eye,
  EyeOff,
  AlertCircle,
  CheckCircle,
  Plug,
  Database,
  Globe,
  Lock,
  Monitor
} from 'lucide-react'

interface ModelConfig {
  provider: 'openai' | 'anthropic' | 'local' | 'custom'
  model: string
  temperature: number
  maxTokens: number
  topP: number
  frequencyPenalty: number
  presencePenalty: number
  systemPrompt: string
  streamingEnabled: boolean
  functionCallingEnabled: boolean
  customEndpoint?: string
  customHeaders?: Record<string, string>
}

interface MCPServer {
  id: string
  name: string
  endpoint: string
  type: 'stdio' | 'http' | 'websocket'
  enabled: boolean
  config: Record<string, any>
  tools: string[]
  status: 'connected' | 'disconnected' | 'error'
}

interface ApiKey {
  service: string
  key: string
  configured: boolean
  masked: boolean
}

export default function ModelConfiguration() {
  const [activeTab, setActiveTab] = useState('models')
  const [modelConfig, setModelConfig] = useState<ModelConfig>({
    provider: 'openai',
    model: 'gpt-4',
    temperature: 0.7,
    maxTokens: 4000,
    topP: 1.0,
    frequencyPenalty: 0,
    presencePenalty: 0,
    systemPrompt: 'You are a helpful AI assistant.',
    streamingEnabled: true,
    functionCallingEnabled: true
  })
  
  const [mcpServers, setMcpServers] = useState<MCPServer[]>([])
  const [apiKeys, setApiKeys] = useState<ApiKey[]>([])
  const [showKeys, setShowKeys] = useState<Record<string, boolean>>({})
  const [isSaving, setIsSaving] = useState(false)
  const [testingConnection, setTestingConnection] = useState<string | null>(null)
  
  // Available models per provider
  const modelOptions = {
    openai: [
      { value: 'gpt-4', label: 'GPT-4' },
      { value: 'gpt-4-turbo', label: 'GPT-4 Turbo' },
      { value: 'gpt-3.5-turbo', label: 'GPT-3.5 Turbo' },
      { value: 'gpt-4o', label: 'GPT-4o' },
      { value: 'gpt-4o-mini', label: 'GPT-4o Mini' }
    ],
    anthropic: [
      { value: 'claude-3-opus-20240229', label: 'Claude 3 Opus' },
      { value: 'claude-3-sonnet-20240229', label: 'Claude 3 Sonnet' },
      { value: 'claude-3-haiku-20240307', label: 'Claude 3 Haiku' },
      { value: 'claude-3-5-sonnet-20241022', label: 'Claude 3.5 Sonnet' }
    ],
    local: [
      { value: 'llama-2-7b', label: 'Llama 2 7B' },
      { value: 'llama-2-13b', label: 'Llama 2 13B' },
      { value: 'mistral-7b', label: 'Mistral 7B' },
      { value: 'custom', label: 'Custom Model' }
    ]
  }

  useEffect(() => {
    loadConfiguration()
  }, [])

  const loadConfiguration = async () => {
    try {
      // Load model configuration
      const modelResponse = await fetch('/api/config/model')
      if (modelResponse.ok) {
        const modelData = await modelResponse.json()
        setModelConfig(prev => ({ ...prev, ...modelData }))
      }

      // Load MCP servers
      const mcpResponse = await fetch('/api/mcp/servers')
      if (mcpResponse.ok) {
        const mcpData = await mcpResponse.json()
        setMcpServers(mcpData.servers || [])
      }

      // Load API keys status
      const keysResponse = await fetch('/api/keys/status')
      if (keysResponse.ok) {
        const keysData = await keysResponse.json()
        setApiKeys(keysData.keys || [])
      }
    } catch (error) {
      console.error('Failed to load configuration:', error)
    }
  }

  const saveModelConfig = async () => {
    setIsSaving(true)
    try {
      const response = await fetch('/api/config/model', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(modelConfig)
      })
      
      if (response.ok) {
        console.log('Model configuration saved')
      }
    } catch (error) {
      console.error('Failed to save model configuration:', error)
    }
    setIsSaving(false)
  }

  const saveApiKey = async (service: string, key: string) => {
    try {
      const response = await fetch('/api/keys', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ service, key })
      })
      
      if (response.ok) {
        await loadConfiguration()
      }
    } catch (error) {
      console.error('Failed to save API key:', error)
    }
  }

  const testConnection = async (provider: string) => {
    setTestingConnection(provider)
    try {
      const response = await fetch(`/api/test/${provider}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ message: 'Hello, this is a test.' })
      })
      
      const result = await response.json()
      console.log('Connection test result:', result)
    } catch (error) {
      console.error('Connection test failed:', error)
    }
    setTestingConnection(null)
  }

  const addMCPServer = async (serverConfig: Partial<MCPServer>) => {
    try {
      const response = await fetch('/api/mcp/servers', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(serverConfig)
      })
      
      if (response.ok) {
        await loadConfiguration()
      }
    } catch (error) {
      console.error('Failed to add MCP server:', error)
    }
  }

  const toggleMCPServer = async (serverId: string, enabled: boolean) => {
    try {
      const response = await fetch(`/api/mcp/servers/${serverId}`, {
        method: 'PATCH',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ enabled })
      })
      
      if (response.ok) {
        setMcpServers(prev => 
          prev.map(server => 
            server.id === serverId ? { ...server, enabled } : server
          )
        )
      }
    } catch (error) {
      console.error('Failed to toggle MCP server:', error)
    }
  }

  return (
    <div className="h-full flex flex-col" data-feedback-target="Model Configuration">
      <div className="flex items-center justify-between p-4 border-b">
        <div>
          <h2 className="text-lg font-semibold">Configuration</h2>
          <p className="text-sm text-muted-foreground">
            Manage AI models, API keys, and MCP connections
          </p>
        </div>
        <Button onClick={saveModelConfig} disabled={isSaving}>
          <Save className="h-4 w-4 mr-2" />
          {isSaving ? 'Saving...' : 'Save All'}
        </Button>
      </div>

      <Tabs value={activeTab} onValueChange={setActiveTab} className="flex-1 flex flex-col">
        <TabsList className="grid w-full grid-cols-4 mx-4 mt-4">
          <TabsTrigger value="models" data-feedback-target="Models Tab">
            <Brain className="h-4 w-4 mr-2" />
            Models
          </TabsTrigger>
          <TabsTrigger value="keys" data-feedback-target="API Keys Tab">
            <Key className="h-4 w-4 mr-2" />
            API Keys
          </TabsTrigger>
          <TabsTrigger value="mcp" data-feedback-target="MCP Tab">
            <Plug className="h-4 w-4 mr-2" />
            MCP Servers
          </TabsTrigger>
          <TabsTrigger value="advanced" data-feedback-target="Advanced Tab">
            <Settings className="h-4 w-4 mr-2" />
            Advanced
          </TabsTrigger>
        </TabsList>

        <div className="flex-1 overflow-y-auto p-4">
          <TabsContent value="models" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>AI Model Configuration</CardTitle>
                <CardDescription>
                  Configure the AI model and parameters for agent interactions
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label htmlFor="provider">Provider</Label>
                    <Select 
                      value={modelConfig.provider} 
                      onValueChange={(value: any) => setModelConfig({...modelConfig, provider: value})}
                    >
                      <SelectTrigger>
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        <SelectItem value="openai">OpenAI</SelectItem>
                        <SelectItem value="anthropic">Anthropic</SelectItem>
                        <SelectItem value="local">Local Models</SelectItem>
                        <SelectItem value="custom">Custom Endpoint</SelectItem>
                      </SelectContent>
                    </Select>
                  </div>
                  
                  <div>
                    <Label htmlFor="model">Model</Label>
                    <Select 
                      value={modelConfig.model} 
                      onValueChange={(value) => setModelConfig({...modelConfig, model: value})}
                    >
                      <SelectTrigger>
                        <SelectValue />
                      </SelectTrigger>
                      <SelectContent>
                        {modelOptions[modelConfig.provider]?.map(option => (
                          <SelectItem key={option.value} value={option.value}>
                            {option.label}
                          </SelectItem>
                        ))}
                      </SelectContent>
                    </Select>
                  </div>
                </div>

                <div className="space-y-4">
                  <div>
                    <div className="flex justify-between mb-2">
                      <Label>Temperature: {modelConfig.temperature}</Label>
                      <span className="text-sm text-muted-foreground">Creativity</span>
                    </div>
                    <Slider
                      value={[modelConfig.temperature]}
                      onValueChange={([value]) => setModelConfig({...modelConfig, temperature: value})}
                      max={2}
                      min={0}
                      step={0.1}
                      className="w-full"
                    />
                  </div>
                  
                  <div>
                    <div className="flex justify-between mb-2">
                      <Label>Max Tokens: {modelConfig.maxTokens}</Label>
                      <span className="text-sm text-muted-foreground">Response length</span>
                    </div>
                    <Slider
                      value={[modelConfig.maxTokens]}
                      onValueChange={([value]) => setModelConfig({...modelConfig, maxTokens: value})}
                      max={8000}
                      min={100}
                      step={100}
                      className="w-full"
                    />
                  </div>
                  
                  <div>
                    <div className="flex justify-between mb-2">
                      <Label>Top P: {modelConfig.topP}</Label>
                      <span className="text-sm text-muted-foreground">Nucleus sampling</span>
                    </div>
                    <Slider
                      value={[modelConfig.topP]}
                      onValueChange={([value]) => setModelConfig({...modelConfig, topP: value})}
                      max={1}
                      min={0}
                      step={0.1}
                      className="w-full"
                    />
                  </div>
                </div>

                <div className="space-y-4">
                  <div>
                    <Label htmlFor="systemPrompt">System Prompt</Label>
                    <Textarea
                      id="systemPrompt"
                      value={modelConfig.systemPrompt}
                      onChange={(e) => setModelConfig({...modelConfig, systemPrompt: e.target.value})}
                      placeholder="Enter system prompt..."
                      rows={3}
                    />
                  </div>
                  
                  <div className="flex items-center justify-between">
                    <div className="space-y-0.5">
                      <Label htmlFor="streaming">Streaming Responses</Label>
                      <p className="text-sm text-muted-foreground">
                        Enable real-time response streaming
                      </p>
                    </div>
                    <Switch
                      id="streaming"
                      checked={modelConfig.streamingEnabled}
                      onCheckedChange={(checked) => setModelConfig({...modelConfig, streamingEnabled: checked})}
                    />
                  </div>
                  
                  <div className="flex items-center justify-between">
                    <div className="space-y-0.5">
                      <Label htmlFor="functionCalling">Function Calling</Label>
                      <p className="text-sm text-muted-foreground">
                        Enable tool and function calling
                      </p>
                    </div>
                    <Switch
                      id="functionCalling"
                      checked={modelConfig.functionCallingEnabled}
                      onCheckedChange={(checked) => setModelConfig({...modelConfig, functionCallingEnabled: checked})}
                    />
                  </div>
                </div>

                <div className="flex gap-2">
                  <Button onClick={() => testConnection(modelConfig.provider)} disabled={testingConnection === modelConfig.provider}>
                    <TestTube className="h-4 w-4 mr-2" />
                    {testingConnection === modelConfig.provider ? 'Testing...' : 'Test Connection'}
                  </Button>
                  <Button variant="outline" onClick={loadConfiguration}>
                    <RefreshCw className="h-4 w-4 mr-2" />
                    Reload
                  </Button>
                </div>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="keys" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>API Keys</CardTitle>
                <CardDescription>
                  Manage API keys for different AI providers
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                {['openai', 'anthropic', 'google', 'azure'].map(service => {
                  const keyData = apiKeys.find(k => k.service === service)
                  return (
                    <div key={service} className="flex items-center justify-between p-4 border rounded-lg">
                      <div className="flex items-center gap-3">
                        <div className="w-10 h-10 rounded-lg bg-gray-100 flex items-center justify-center">
                          <Key className="h-5 w-5" />
                        </div>
                        <div>
                          <div className="font-medium capitalize">{service}</div>
                          <div className="text-sm text-muted-foreground">
                            {keyData?.configured ? (
                              <div className="flex items-center gap-1 text-green-600">
                                <CheckCircle className="h-3 w-3" />
                                Configured
                              </div>
                            ) : (
                              <div className="flex items-center gap-1 text-red-600">
                                <AlertCircle className="h-3 w-3" />
                                Not configured
                              </div>
                            )}
                          </div>
                        </div>
                      </div>
                      <div className="flex items-center gap-2">
                        <Input
                          type={showKeys[service] ? 'text' : 'password'}
                          placeholder="Enter API key..."
                          value={keyData?.key || ''}
                          onChange={(e) => {
                            const newKeys = apiKeys.map(k => 
                              k.service === service ? { ...k, key: e.target.value } : k
                            )
                            if (!newKeys.find(k => k.service === service)) {
                              newKeys.push({ service, key: e.target.value, configured: false, masked: false })
                            }
                            setApiKeys(newKeys)
                          }}
                          className="w-64"
                        />
                        <Button
                          variant="ghost"
                          size="sm"
                          onClick={() => setShowKeys(prev => ({ ...prev, [service]: !prev[service] }))}
                        >
                          {showKeys[service] ? <EyeOff className="h-4 w-4" /> : <Eye className="h-4 w-4" />}
                        </Button>
                        <Button
                          size="sm"
                          onClick={() => saveApiKey(service, keyData?.key || '')}
                          disabled={!keyData?.key}
                        >
                          Save
                        </Button>
                      </div>
                    </div>
                  )
                })}
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="mcp" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>MCP Servers</CardTitle>
                <CardDescription>
                  Manage Model Context Protocol server connections
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                {mcpServers.map(server => (
                  <div key={server.id} className="flex items-center justify-between p-4 border rounded-lg">
                    <div className="flex items-center gap-3">
                      <div className="w-10 h-10 rounded-lg bg-gray-100 flex items-center justify-center">
                        <Server className="h-5 w-5" />
                      </div>
                      <div>
                        <div className="font-medium">{server.name}</div>
                        <div className="text-sm text-muted-foreground">{server.endpoint}</div>
                        <div className="flex items-center gap-2 mt-1">
                          <Badge variant={server.status === 'connected' ? 'default' : 'destructive'}>
                            {server.status}
                          </Badge>
                          <Badge variant="outline">{server.type}</Badge>
                          <span className="text-xs text-muted-foreground">
                            {server.tools.length} tools
                          </span>
                        </div>
                      </div>
                    </div>
                    <div className="flex items-center gap-2">
                      <Switch
                        checked={server.enabled}
                        onCheckedChange={(checked) => toggleMCPServer(server.id, checked)}
                      />
                      <Button variant="outline" size="sm">
                        Configure
                      </Button>
                    </div>
                  </div>
                ))}
                
                <Button onClick={() => addMCPServer({})}>
                  <Plug className="h-4 w-4 mr-2" />
                  Add MCP Server
                </Button>
              </CardContent>
            </Card>
          </TabsContent>

          <TabsContent value="advanced" className="space-y-6">
            <Card>
              <CardHeader>
                <CardTitle>Advanced Settings</CardTitle>
                <CardDescription>
                  Advanced configuration options and system settings
                </CardDescription>
              </CardHeader>
              <CardContent className="space-y-4">
                <div className="grid grid-cols-2 gap-4">
                  <div>
                    <Label>Frequency Penalty: {modelConfig.frequencyPenalty}</Label>
                    <Slider
                      value={[modelConfig.frequencyPenalty]}
                      onValueChange={([value]) => setModelConfig({...modelConfig, frequencyPenalty: value})}
                      max={2}
                      min={-2}
                      step={0.1}
                      className="w-full mt-2"
                    />
                  </div>
                  
                  <div>
                    <Label>Presence Penalty: {modelConfig.presencePenalty}</Label>
                    <Slider
                      value={[modelConfig.presencePenalty]}
                      onValueChange={([value]) => setModelConfig({...modelConfig, presencePenalty: value})}
                      max={2}
                      min={-2}
                      step={0.1}
                      className="w-full mt-2"
                    />
                  </div>
                </div>
                
                {modelConfig.provider === 'custom' && (
                  <div className="space-y-4">
                    <div>
                      <Label htmlFor="customEndpoint">Custom Endpoint</Label>
                      <Input
                        id="customEndpoint"
                        value={modelConfig.customEndpoint || ''}
                        onChange={(e) => setModelConfig({...modelConfig, customEndpoint: e.target.value})}
                        placeholder="https://api.example.com/v1/completions"
                      />
                    </div>
                    
                    <div>
                      <Label htmlFor="customHeaders">Custom Headers (JSON)</Label>
                      <Textarea
                        id="customHeaders"
                        value={JSON.stringify(modelConfig.customHeaders || {}, null, 2)}
                        onChange={(e) => {
                          try {
                            const headers = JSON.parse(e.target.value)
                            setModelConfig({...modelConfig, customHeaders: headers})
                          } catch (error) {
                            // Invalid JSON, don't update
                          }
                        }}
                        placeholder='{"Authorization": "Bearer token", "Custom-Header": "value"}'
                        rows={4}
                      />
                    </div>
                  </div>
                )}
              </CardContent>
            </Card>
          </TabsContent>
        </div>
      </Tabs>
    </div>
  )
}