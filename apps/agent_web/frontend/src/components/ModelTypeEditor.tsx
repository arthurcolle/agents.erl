import { useState, useEffect } from 'react'
import { Card, CardContent, CardDescription, CardHeader, CardTitle } from '@/components/ui/card'
import { Button } from '@/components/ui/button'
import { Input } from '@/components/ui/input'
import { Label } from '@/components/ui/label'
import { Textarea } from '@/components/ui/textarea'
import { Badge } from '@/components/ui/badge'
import { Tabs, TabsContent, TabsList, TabsTrigger } from '@/components/ui/tabs'
import { ScrollArea } from '@/components/ui/scroll-area'
import { Select, SelectContent, SelectItem, SelectTrigger, SelectValue } from '@/components/ui/select'
import { Switch } from '@/components/ui/switch'
import { Collapsible, CollapsibleContent, CollapsibleTrigger } from '@/components/ui/collapsible'
import { 
  Plus, 
  Trash2, 
  Edit, 
  Save, 
  Code, 
  Database, 
  Search,
  Settings,
  ChevronDown,
  ChevronRight,
  Copy,
  Play,
  Brain,
  Network,
  FileText,
  Hash,
  Calendar,
  MapPin,
  Type,
  ToggleLeft
} from 'lucide-react'
import { cn } from '@/lib/utils'

// Type definitions
interface FieldMetadata {
  index_type: 'none' | 'fulltext' | 'vector' | 'exact' | 'numeric' | 'geo' | 'datetime'
  embedding_model?: 'none' | 'jina-clip-v2' | 'openai-text' | 'text-embedding-ada-002' | 'custom'
  storage_backend: 'graph' | 'vector_db' | 'relational' | 'document' | 'memory'
  metric_type: 'cosine' | 'dot' | 'euclidean' | 'manhattan'
  tokenizer?: string
  embedding_dim?: number
  custom_embedder?: string
  custom_index_settings: Record<string, any>
}

interface ModelField {
  id: string
  name: string
  type: string
  required: boolean
  description: string
  default_value?: any
  metadata: FieldMetadata
  validation_rules: string[]
}

interface ModelDefinition {
  id: string
  name: string
  description: string
  fields: ModelField[]
  config: {
    default_index_type: string
    default_embedding_model?: string
    default_storage_backend: string
    default_metric_type: string
  }
  relationships: {
    id: string
    target_model: string
    relationship_type: string
    description: string
  }[]
  created_at: Date
  updated_at: Date
  version: string
}

export default function ModelTypeEditor() {
  const [models, setModels] = useState<ModelDefinition[]>([])
  const [selectedModel, setSelectedModel] = useState<ModelDefinition | null>(null)
  const [editingModel, setEditingModel] = useState<ModelDefinition | null>(null)
  const [isCreating, setIsCreating] = useState(false)
  const [activeTab, setActiveTab] = useState('models')

  // Load models
  useEffect(() => {
    loadModels()
  }, [])

  const loadModels = async () => {
    try {
      const response = await fetch('/api/models/definitions')
      if (response.ok) {
        const data = await response.json()
        setModels(data.models || [])
      } else {
        // Load mock data
        loadMockModels()
      }
    } catch (error) {
      console.error('Failed to load models:', error)
      loadMockModels()
    }
  }

  const loadMockModels = () => {
    const mockModels: ModelDefinition[] = [
      {
        id: 'user_model',
        name: 'User',
        description: 'User model with profile information and retrieval capabilities',
        fields: [
          {
            id: 'username',
            name: 'username',
            type: 'string',
            required: true,
            description: 'Unique username for the user',
            metadata: {
              index_type: 'exact',
              storage_backend: 'graph',
              metric_type: 'cosine',
              custom_index_settings: {}
            },
            validation_rules: ['min_length:3', 'max_length:50', 'alphanumeric']
          },
          {
            id: 'email',
            name: 'email',
            type: 'string',
            required: true,
            description: 'User email address',
            metadata: {
              index_type: 'exact',
              storage_backend: 'graph',
              metric_type: 'cosine',
              custom_index_settings: {}
            },
            validation_rules: ['email_format']
          },
          {
            id: 'bio',
            name: 'bio',
            type: 'string',
            required: false,
            description: 'User biography/description',
            metadata: {
              index_type: 'vector',
              embedding_model: 'jina-clip-v2',
              storage_backend: 'vector_db',
              metric_type: 'cosine',
              embedding_dim: 512,
              tokenizer: 'en_core_web_sm',
              custom_index_settings: {}
            },
            validation_rules: ['max_length:1000']
          },
          {
            id: 'age',
            name: 'age',
            type: 'integer',
            required: false,
            description: 'User age',
            metadata: {
              index_type: 'numeric',
              storage_backend: 'graph',
              metric_type: 'cosine',
              custom_index_settings: {}
            },
            validation_rules: ['min:13', 'max:120']
          }
        ],
        config: {
          default_index_type: 'none',
          default_embedding_model: 'jina-clip-v2',
          default_storage_backend: 'graph',
          default_metric_type: 'cosine'
        },
        relationships: [
          {
            id: 'user_posts',
            target_model: 'Post',
            relationship_type: 'one_to_many',
            description: 'Posts created by this user'
          }
        ],
        created_at: new Date(),
        updated_at: new Date(),
        version: '1.0.0'
      },
      {
        id: 'post_model',
        name: 'Post',
        description: 'Blog post model with content indexing',
        fields: [
          {
            id: 'title',
            name: 'title',
            type: 'string',
            required: true,
            description: 'Post title',
            metadata: {
              index_type: 'fulltext',
              embedding_model: 'openai-text',
              storage_backend: 'vector_db',
              metric_type: 'cosine',
              embedding_dim: 1536,
              custom_index_settings: {}
            },
            validation_rules: ['min_length:5', 'max_length:200']
          },
          {
            id: 'content',
            name: 'content',
            type: 'text',
            required: true,
            description: 'Post content',
            metadata: {
              index_type: 'vector',
              embedding_model: 'jina-clip-v2',
              storage_backend: 'vector_db',
              metric_type: 'cosine',
              embedding_dim: 768,
              custom_index_settings: { chunk_size: 500, overlap: 50 }
            },
            validation_rules: ['min_length:50']
          },
          {
            id: 'created_at',
            name: 'created_at',
            type: 'datetime',
            required: true,
            description: 'Creation timestamp',
            metadata: {
              index_type: 'datetime',
              storage_backend: 'graph',
              metric_type: 'cosine',
              custom_index_settings: {}
            },
            validation_rules: []
          }
        ],
        config: {
          default_index_type: 'vector',
          default_embedding_model: 'jina-clip-v2',
          default_storage_backend: 'vector_db',
          default_metric_type: 'cosine'
        },
        relationships: [
          {
            id: 'post_author',
            target_model: 'User',
            relationship_type: 'many_to_one',
            description: 'Author of this post'
          }
        ],
        created_at: new Date(),
        updated_at: new Date(),
        version: '1.0.0'
      }
    ]
    setModels(mockModels)
  }

  // Create new model
  const createNewModel = () => {
    const newModel: ModelDefinition = {
      id: `model_${Date.now()}`,
      name: '',
      description: '',
      fields: [],
      config: {
        default_index_type: 'none',
        default_storage_backend: 'graph',
        default_metric_type: 'cosine'
      },
      relationships: [],
      created_at: new Date(),
      updated_at: new Date(),
      version: '1.0.0'
    }
    setEditingModel(newModel)
    setIsCreating(true)
  }

  // Add field to model
  const addField = () => {
    if (!editingModel) return

    const newField: ModelField = {
      id: `field_${Date.now()}`,
      name: '',
      type: 'string',
      required: false,
      description: '',
      metadata: {
        index_type: 'none',
        storage_backend: 'graph',
        metric_type: 'cosine',
        custom_index_settings: {}
      },
      validation_rules: []
    }

    setEditingModel({
      ...editingModel,
      fields: [...editingModel.fields, newField]
    })
  }

  // Remove field from model
  const removeField = (fieldId: string) => {
    if (!editingModel) return

    setEditingModel({
      ...editingModel,
      fields: editingModel.fields.filter(f => f.id !== fieldId)
    })
  }

  // Update field
  const updateField = (fieldId: string, updates: Partial<ModelField>) => {
    if (!editingModel) return

    setEditingModel({
      ...editingModel,
      fields: editingModel.fields.map(f =>
        f.id === fieldId ? { ...f, ...updates } : f
      )
    })
  }

  // Save model
  const saveModel = async () => {
    if (!editingModel) return

    try {
      const method = isCreating ? 'POST' : 'PUT'
      const url = isCreating ? '/api/models/definitions' : `/api/models/definitions/${editingModel.id}`
      
      const response = await fetch(url, {
        method,
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(editingModel)
      })

      if (response.ok) {
        if (isCreating) {
          setModels(prev => [...prev, editingModel])
        } else {
          setModels(prev => prev.map(m => m.id === editingModel.id ? editingModel : m))
        }
        setEditingModel(null)
        setIsCreating(false)
        setSelectedModel(editingModel)
      }
    } catch (error) {
      console.error('Failed to save model:', error)
    }
  }

  // Cancel editing
  const cancelEdit = () => {
    setEditingModel(null)
    setIsCreating(false)
  }

  // Generate model code
  const generateModelCode = (model: ModelDefinition) => {
    const fields = model.fields.map(field => {
      const metadata = field.metadata
      const metadataStr = Object.entries(metadata)
        .filter(([_, value]) => value !== undefined && value !== null)
        .map(([key, value]) => `${key}=${JSON.stringify(value)}`)
        .join(', ')
      
      const requiredStr = field.required ? 'required=True' : 'required=False'
      const typeStr = field.type === 'string' ? 'str' : 
                    field.type === 'integer' ? 'int' : 
                    field.type === 'boolean' ? 'bool' : 
                    field.type === 'datetime' ? 'datetime' :
                    field.type === 'text' ? 'str' : field.type

      return `    ${field.name}: ${typeStr} = retrieval_field(${metadataStr}, ${requiredStr})`
    }).join('\n')

    return `class ${model.name}(BasedModel):
    """${model.description}"""
    
${fields}

    class Config:
        default_index_type = IndexType.${model.config.default_index_type.toUpperCase()}
        default_storage_backend = StorageBackend.${model.config.default_storage_backend.toUpperCase()}
        default_metric_type = MetricType.${model.config.default_metric_type.toUpperCase()}`
  }

  const getFieldTypeIcon = (type: string) => {
    switch (type) {
      case 'string': case 'text': return <Type className="h-4 w-4" />
      case 'integer': case 'float': return <Hash className="h-4 w-4" />
      case 'boolean': return <ToggleLeft className="h-4 w-4" />
      case 'datetime': return <Calendar className="h-4 w-4" />
      case 'geo': return <MapPin className="h-4 w-4" />
      default: return <FileText className="h-4 w-4" />
    }
  }

  const getIndexTypeIcon = (indexType: string) => {
    switch (indexType) {
      case 'fulltext': return <Search className="h-4 w-4" />
      case 'vector': return <Brain className="h-4 w-4" />
      case 'exact': return <Type className="h-4 w-4" />
      case 'numeric': return <Hash className="h-4 w-4" />
      case 'geo': return <MapPin className="h-4 w-4" />
      case 'datetime': return <Calendar className="h-4 w-4" />
      default: return <Database className="h-4 w-4" />
    }
  }

  return (
    <div className="h-full flex">
      {/* Model List Sidebar */}
      <div className="w-80 border-r bg-gray-50">
        <div className="p-4 border-b">
          <div className="flex items-center justify-between mb-4">
            <h2 className="text-lg font-semibold">Model Types</h2>
            <Button onClick={createNewModel} size="sm">
              <Plus className="h-4 w-4 mr-1" />
              New Model
            </Button>
          </div>
          <div className="text-sm text-gray-600">
            {models.length} models defined
          </div>
        </div>
        
        <ScrollArea className="h-[calc(100vh-120px)]">
          <div className="p-2 space-y-2">
            {models.map(model => (
              <Card
                key={model.id}
                className={cn(
                  "cursor-pointer transition-colors hover:bg-blue-50",
                  selectedModel?.id === model.id && "bg-blue-100 border-blue-300"
                )}
                onClick={() => setSelectedModel(model)}
              >
                <CardContent className="p-3">
                  <div className="flex items-center justify-between">
                    <div>
                      <h3 className="font-medium">{model.name}</h3>
                      <p className="text-xs text-gray-500 truncate">
                        {model.description}
                      </p>
                    </div>
                    <Badge variant="outline" className="text-xs">
                      {model.fields.length} fields
                    </Badge>
                  </div>
                  <div className="flex items-center gap-1 mt-2">
                    <Badge variant="secondary" className="text-xs">
                      v{model.version}
                    </Badge>
                    <Badge variant="outline" className="text-xs">
                      {model.config.default_storage_backend}
                    </Badge>
                  </div>
                </CardContent>
              </Card>
            ))}
          </div>
        </ScrollArea>
      </div>

      {/* Main Content */}
      <div className="flex-1 flex flex-col">
        {editingModel ? (
          // Edit Mode
          <div className="h-full flex flex-col">
            <div className="p-4 border-b bg-yellow-50">
              <div className="flex items-center justify-between">
                <h2 className="text-lg font-semibold">
                  {isCreating ? 'Create New Model' : `Edit ${editingModel.name}`}
                </h2>
                <div className="flex gap-2">
                  <Button variant="outline" onClick={cancelEdit}>
                    Cancel
                  </Button>
                  <Button onClick={saveModel}>
                    <Save className="h-4 w-4 mr-1" />
                    Save
                  </Button>
                </div>
              </div>
            </div>

            <div className="flex-1 overflow-y-auto p-4">
              <Tabs defaultValue="basic" className="space-y-4">
                <TabsList>
                  <TabsTrigger value="basic">Basic Info</TabsTrigger>
                  <TabsTrigger value="fields">Fields</TabsTrigger>
                  <TabsTrigger value="config">Configuration</TabsTrigger>
                  <TabsTrigger value="relationships">Relationships</TabsTrigger>
                </TabsList>

                <TabsContent value="basic" className="space-y-4">
                  <div className="grid grid-cols-1 gap-4">
                    <div>
                      <Label htmlFor="name">Model Name</Label>
                      <Input
                        id="name"
                        value={editingModel.name}
                        onChange={(e) => setEditingModel({
                          ...editingModel,
                          name: e.target.value
                        })}
                        placeholder="e.g., User, Post, Product"
                      />
                    </div>
                    <div>
                      <Label htmlFor="description">Description</Label>
                      <Textarea
                        id="description"
                        value={editingModel.description}
                        onChange={(e) => setEditingModel({
                          ...editingModel,
                          description: e.target.value
                        })}
                        placeholder="Describe what this model represents..."
                      />
                    </div>
                    <div>
                      <Label htmlFor="version">Version</Label>
                      <Input
                        id="version"
                        value={editingModel.version}
                        onChange={(e) => setEditingModel({
                          ...editingModel,
                          version: e.target.value
                        })}
                        placeholder="1.0.0"
                      />
                    </div>
                  </div>
                </TabsContent>

                <TabsContent value="fields" className="space-y-4">
                  <div className="flex items-center justify-between">
                    <h3 className="text-lg font-medium">Model Fields</h3>
                    <Button onClick={addField}>
                      <Plus className="h-4 w-4 mr-1" />
                      Add Field
                    </Button>
                  </div>

                  <div className="space-y-4">
                    {editingModel.fields.map(field => (
                      <Card key={field.id}>
                        <Collapsible>
                          <CollapsibleTrigger className="w-full">
                            <CardHeader className="py-3">
                              <div className="flex items-center justify-between">
                                <div className="flex items-center gap-2">
                                  <ChevronRight className="h-4 w-4" />
                                  {getFieldTypeIcon(field.type)}
                                  <div className="text-left">
                                    <div className="font-medium">
                                      {field.name || 'Unnamed Field'}
                                    </div>
                                    <div className="text-sm text-gray-500">
                                      {field.type} • {field.metadata.index_type}
                                    </div>
                                  </div>
                                </div>
                                <div className="flex items-center gap-2">
                                  {field.required && (
                                    <Badge variant="destructive" className="text-xs">
                                      Required
                                    </Badge>
                                  )}
                                  <Badge variant="outline" className="text-xs">
                                    {field.metadata.index_type}
                                  </Badge>
                                  <Button
                                    variant="ghost"
                                    size="sm"
                                    onClick={(e) => {
                                      e.stopPropagation()
                                      removeField(field.id)
                                    }}
                                  >
                                    <Trash2 className="h-4 w-4" />
                                  </Button>
                                </div>
                              </div>
                            </CardHeader>
                          </CollapsibleTrigger>
                          <CollapsibleContent>
                            <CardContent className="pt-0 space-y-4">
                              <div className="grid grid-cols-2 gap-4">
                                <div>
                                  <Label>Field Name</Label>
                                  <Input
                                    value={field.name}
                                    onChange={(e) => updateField(field.id, { name: e.target.value })}
                                    placeholder="field_name"
                                  />
                                </div>
                                <div>
                                  <Label>Type</Label>
                                  <Select
                                    value={field.type}
                                    onValueChange={(value) => updateField(field.id, { type: value })}
                                  >
                                    <SelectTrigger>
                                      <SelectValue />
                                    </SelectTrigger>
                                    <SelectContent>
                                      <SelectItem value="string">String</SelectItem>
                                      <SelectItem value="text">Text (Long)</SelectItem>
                                      <SelectItem value="integer">Integer</SelectItem>
                                      <SelectItem value="float">Float</SelectItem>
                                      <SelectItem value="boolean">Boolean</SelectItem>
                                      <SelectItem value="datetime">DateTime</SelectItem>
                                      <SelectItem value="geo">Geolocation</SelectItem>
                                    </SelectContent>
                                  </Select>
                                </div>
                              </div>

                              <div>
                                <Label>Description</Label>
                                <Input
                                  value={field.description}
                                  onChange={(e) => updateField(field.id, { description: e.target.value })}
                                  placeholder="Describe this field..."
                                />
                              </div>

                              <div className="flex items-center space-x-2">
                                <Switch
                                  checked={field.required}
                                  onCheckedChange={(checked) => updateField(field.id, { required: checked })}
                                />
                                <Label>Required field</Label>
                              </div>

                              <div className="grid grid-cols-2 gap-4">
                                <div>
                                  <Label>Index Type</Label>
                                  <Select
                                    value={field.metadata.index_type}
                                    onValueChange={(value) => updateField(field.id, {
                                      metadata: { ...field.metadata, index_type: value as any }
                                    })}
                                  >
                                    <SelectTrigger>
                                      <SelectValue />
                                    </SelectTrigger>
                                    <SelectContent>
                                      <SelectItem value="none">None</SelectItem>
                                      <SelectItem value="exact">Exact Match</SelectItem>
                                      <SelectItem value="fulltext">Full Text</SelectItem>
                                      <SelectItem value="vector">Vector Search</SelectItem>
                                      <SelectItem value="numeric">Numeric Range</SelectItem>
                                      <SelectItem value="geo">Geospatial</SelectItem>
                                      <SelectItem value="datetime">Date/Time</SelectItem>
                                    </SelectContent>
                                  </Select>
                                </div>

                                {field.metadata.index_type === 'vector' && (
                                  <div>
                                    <Label>Embedding Model</Label>
                                    <Select
                                      value={field.metadata.embedding_model || 'none'}
                                      onValueChange={(value) => updateField(field.id, {
                                        metadata: { ...field.metadata, embedding_model: value as any }
                                      })}
                                    >
                                      <SelectTrigger>
                                        <SelectValue />
                                      </SelectTrigger>
                                      <SelectContent>
                                        <SelectItem value="none">None</SelectItem>
                                        <SelectItem value="jina-clip-v2">Jina CLIP v2</SelectItem>
                                        <SelectItem value="openai-text">OpenAI Text</SelectItem>
                                        <SelectItem value="text-embedding-ada-002">OpenAI Ada</SelectItem>
                                        <SelectItem value="custom">Custom</SelectItem>
                                      </SelectContent>
                                    </Select>
                                  </div>
                                )}
                              </div>
                            </CardContent>
                          </CollapsibleContent>
                        </Collapsible>
                      </Card>
                    ))}
                  </div>
                </TabsContent>

                <TabsContent value="config" className="space-y-4">
                  <div className="grid grid-cols-2 gap-4">
                    <div>
                      <Label>Default Index Type</Label>
                      <Select
                        value={editingModel.config.default_index_type}
                        onValueChange={(value) => setEditingModel({
                          ...editingModel,
                          config: { ...editingModel.config, default_index_type: value }
                        })}
                      >
                        <SelectTrigger>
                          <SelectValue />
                        </SelectTrigger>
                        <SelectContent>
                          <SelectItem value="none">None</SelectItem>
                          <SelectItem value="exact">Exact</SelectItem>
                          <SelectItem value="fulltext">Full Text</SelectItem>
                          <SelectItem value="vector">Vector</SelectItem>
                        </SelectContent>
                      </Select>
                    </div>

                    <div>
                      <Label>Default Storage Backend</Label>
                      <Select
                        value={editingModel.config.default_storage_backend}
                        onValueChange={(value) => setEditingModel({
                          ...editingModel,
                          config: { ...editingModel.config, default_storage_backend: value }
                        })}
                      >
                        <SelectTrigger>
                          <SelectValue />
                        </SelectTrigger>
                        <SelectContent>
                          <SelectItem value="graph">Graph</SelectItem>
                          <SelectItem value="vector_db">Vector DB</SelectItem>
                          <SelectItem value="relational">Relational</SelectItem>
                          <SelectItem value="document">Document</SelectItem>
                          <SelectItem value="memory">Memory</SelectItem>
                        </SelectContent>
                      </Select>
                    </div>
                  </div>
                </TabsContent>

                <TabsContent value="relationships" className="space-y-4">
                  <div className="flex items-center justify-between">
                    <h3 className="text-lg font-medium">Model Relationships</h3>
                    <Button>
                      <Plus className="h-4 w-4 mr-1" />
                      Add Relationship
                    </Button>
                  </div>
                  <div className="text-center text-gray-500 py-8">
                    Relationship management coming soon
                  </div>
                </TabsContent>
              </Tabs>
            </div>
          </div>
        ) : selectedModel ? (
          // View Mode
          <div className="h-full flex flex-col">
            <div className="p-4 border-b">
              <div className="flex items-center justify-between">
                <div>
                  <h2 className="text-xl font-bold">{selectedModel.name}</h2>
                  <p className="text-gray-600">{selectedModel.description}</p>
                </div>
                <div className="flex gap-2">
                  <Button
                    variant="outline"
                    onClick={() => setEditingModel(selectedModel)}
                  >
                    <Edit className="h-4 w-4 mr-1" />
                    Edit
                  </Button>
                  <Button>
                    <Play className="h-4 w-4 mr-1" />
                    Deploy
                  </Button>
                </div>
              </div>
            </div>

            <div className="flex-1 overflow-y-auto">
              <Tabs defaultValue="fields" className="p-4">
                <TabsList>
                  <TabsTrigger value="fields">Fields ({selectedModel.fields.length})</TabsTrigger>
                  <TabsTrigger value="code">Generated Code</TabsTrigger>
                  <TabsTrigger value="instances">Instances</TabsTrigger>
                </TabsList>

                <TabsContent value="fields" className="space-y-4">
                  <div className="grid gap-4">
                    {selectedModel.fields.map(field => (
                      <Card key={field.id}>
                        <CardContent className="p-4">
                          <div className="flex items-start justify-between">
                            <div className="flex items-start gap-3">
                              {getFieldTypeIcon(field.type)}
                              <div>
                                <div className="flex items-center gap-2">
                                  <h3 className="font-medium">{field.name}</h3>
                                  {field.required && (
                                    <Badge variant="destructive" className="text-xs">
                                      Required
                                    </Badge>
                                  )}
                                </div>
                                <p className="text-sm text-gray-600 mt-1">
                                  {field.description}
                                </p>
                                <div className="flex items-center gap-2 mt-2">
                                  <Badge variant="outline" className="text-xs">
                                    {field.type}
                                  </Badge>
                                  <Badge variant="secondary" className="text-xs flex items-center gap-1">
                                    {getIndexTypeIcon(field.metadata.index_type)}
                                    {field.metadata.index_type}
                                  </Badge>
                                  {field.metadata.embedding_model && (
                                    <Badge variant="outline" className="text-xs">
                                      {field.metadata.embedding_model}
                                    </Badge>
                                  )}
                                </div>
                              </div>
                            </div>
                          </div>
                        </CardContent>
                      </Card>
                    ))}
                  </div>
                </TabsContent>

                <TabsContent value="code">
                  <Card>
                    <CardHeader>
                      <div className="flex items-center justify-between">
                        <CardTitle>Generated Python Code</CardTitle>
                        <Button
                          variant="outline"
                          size="sm"
                          onClick={() => navigator.clipboard.writeText(generateModelCode(selectedModel))}
                        >
                          <Copy className="h-4 w-4 mr-1" />
                          Copy
                        </Button>
                      </div>
                    </CardHeader>
                    <CardContent>
                      <pre className="bg-gray-900 text-gray-100 p-4 rounded-lg overflow-x-auto text-sm">
                        <code>{generateModelCode(selectedModel)}</code>
                      </pre>
                    </CardContent>
                  </Card>
                </TabsContent>

                <TabsContent value="instances">
                  <div className="text-center py-8 text-gray-500">
                    Model instances will appear here once the model is deployed
                  </div>
                </TabsContent>
              </Tabs>
            </div>
          </div>
        ) : (
          // No Selection
          <div className="flex-1 flex items-center justify-center">
            <div className="text-center">
              <Code className="h-16 w-16 text-gray-400 mx-auto mb-4" />
              <h3 className="text-lg font-medium text-gray-700 mb-2">No Model Selected</h3>
              <p className="text-gray-500 mb-4">
                Select a model from the sidebar or create a new one
              </p>
              <Button onClick={createNewModel}>
                <Plus className="h-4 w-4 mr-1" />
                Create New Model
              </Button>
            </div>
          </div>
        )}
      </div>
    </div>
  )
}