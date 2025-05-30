interface VectorEmbedding {
  id: string
  vector: number[]
  metadata: {
    content: string
    type: 'message' | 'code' | 'document' | 'knowledge'
    timestamp: number
    context: string[]
    userId?: string
    sessionId?: string
  }
}

interface SemanticSearchResult {
  id: string
  content: string
  similarity: number
  type: string
  context: string[]
  relevantChunks: string[]
}

interface ContextualRetrievalConfig {
  maxResults: number
  similarityThreshold: number
  contextWindow: number
  enableSemanticCaching: boolean
  adaptiveRetrieval: boolean
}

export class ContextualRetrievalSystem {
  private embeddings: Map<string, VectorEmbedding> = new Map()
  private semanticCache: Map<string, SemanticSearchResult[]> = new Map()
  private contextGraph: Map<string, Set<string>> = new Map()
  private config: ContextualRetrievalConfig
  private ws: WebSocket | null = null

  constructor(config: Partial<ContextualRetrievalConfig> = {}) {
    this.config = {
      maxResults: 10,
      similarityThreshold: 0.7,
      contextWindow: 5,
      enableSemanticCaching: true,
      adaptiveRetrieval: true,
      ...config
    }
  }

  // Initialize with WebSocket for real-time updates
  async initialize(ws: WebSocket) {
    this.ws = ws
    await this.loadExistingEmbeddings()
    this.setupRealTimeSync()
  }

  // Advanced semantic search with contextual awareness
  async semanticSearch(
    query: string, 
    context: string[] = [],
    options: {
      filterByType?: string[]
      boostRecent?: boolean
      includeRelated?: boolean
      dynamicThreshold?: boolean
    } = {}
  ): Promise<SemanticSearchResult[]> {
    
    // Check semantic cache first
    const cacheKey = this.generateCacheKey(query, context, options)
    if (this.config.enableSemanticCaching && this.semanticCache.has(cacheKey)) {
      return this.semanticCache.get(cacheKey)!
    }

    // Generate query embedding
    const queryEmbedding = await this.generateEmbedding(query)
    
    // Calculate similarities with contextual boosting
    const similarities: Array<{
      embedding: VectorEmbedding
      similarity: number
      contextBoost: number
      recencyBoost: number
    }> = []

    for (const [id, embedding] of this.embeddings) {
      // Skip if type filtering is enabled
      if (options.filterByType && !options.filterByType.includes(embedding.metadata.type)) {
        continue
      }

      const baseSimilarity = this.cosineSimilarity(queryEmbedding, embedding.vector)
      
      // Calculate contextual relevance boost
      const contextBoost = this.calculateContextualBoost(context, embedding.metadata.context)
      
      // Calculate recency boost if enabled
      const recencyBoost = options.boostRecent 
        ? this.calculateRecencyBoost(embedding.metadata.timestamp)
        : 1

      const finalSimilarity = baseSimilarity * (1 + contextBoost * 0.3) * recencyBoost

      similarities.push({
        embedding,
        similarity: finalSimilarity,
        contextBoost,
        recencyBoost
      })
    }

    // Dynamic threshold adjustment based on query complexity
    let threshold = this.config.similarityThreshold
    if (options.dynamicThreshold) {
      threshold = this.adaptThreshold(query, similarities)
    }

    // Sort and filter results
    const filteredResults = similarities
      .filter(item => item.similarity >= threshold)
      .sort((a, b) => b.similarity - a.similarity)
      .slice(0, this.config.maxResults)

    // Convert to search results with relevant chunks
    const results: SemanticSearchResult[] = await Promise.all(
      filteredResults.map(async (item) => {
        const relevantChunks = await this.extractRelevantChunks(
          query, 
          item.embedding.metadata.content
        )

        return {
          id: item.embedding.id,
          content: item.embedding.metadata.content,
          similarity: item.similarity,
          type: item.embedding.metadata.type,
          context: item.embedding.metadata.context,
          relevantChunks
        }
      })
    )

    // Include related content if requested
    if (options.includeRelated) {
      const relatedResults = await this.findRelatedContent(results)
      results.push(...relatedResults)
    }

    // Cache results
    if (this.config.enableSemanticCaching) {
      this.semanticCache.set(cacheKey, results)
    }

    return results
  }

  // Store content with automatic embedding generation
  async storeContent(
    content: string,
    type: 'message' | 'code' | 'document' | 'knowledge',
    context: string[] = [],
    metadata: any = {}
  ): Promise<string> {
    const id = this.generateId()
    const embedding = await this.generateEmbedding(content)
    
    const vectorEmbedding: VectorEmbedding = {
      id,
      vector: embedding,
      metadata: {
        content,
        type,
        timestamp: Date.now(),
        context,
        ...metadata
      }
    }

    this.embeddings.set(id, vectorEmbedding)
    this.updateContextGraph(id, context)
    
    // Sync to backend if WebSocket is available
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify({
        type: 'store_embedding',
        embedding: vectorEmbedding
      }))
    }

    return id
  }

  // Contextual conversation memory
  async getConversationContext(
    sessionId: string,
    maxMessages: number = 10
  ): Promise<SemanticSearchResult[]> {
    const sessionEmbeddings = Array.from(this.embeddings.values())
      .filter(e => e.metadata.sessionId === sessionId)
      .sort((a, b) => b.metadata.timestamp - a.metadata.timestamp)
      .slice(0, maxMessages)

    return sessionEmbeddings.map(e => ({
      id: e.id,
      content: e.metadata.content,
      similarity: 1.0,
      type: e.metadata.type,
      context: e.metadata.context,
      relevantChunks: []
    }))
  }

  // Auto-suggest related content and actions
  async generateSuggestions(
    currentMessage: string,
    conversationHistory: string[],
    userPreferences: any = {}
  ): Promise<{
    relatedContent: SemanticSearchResult[]
    suggestedActions: string[]
    contextualInsights: string[]
  }> {
    
    // Search for related content
    const relatedContent = await this.semanticSearch(
      currentMessage,
      conversationHistory.slice(-3), // Last 3 messages as context
      {
        filterByType: ['knowledge', 'document', 'code'],
        boostRecent: true,
        includeRelated: true,
        dynamicThreshold: true
      }
    )

    // Generate action suggestions based on content analysis
    const suggestedActions = this.generateActionSuggestions(
      currentMessage,
      relatedContent,
      userPreferences
    )

    // Generate contextual insights
    const contextualInsights = this.generateContextualInsights(
      currentMessage,
      relatedContent,
      conversationHistory
    )

    return {
      relatedContent,
      suggestedActions,
      contextualInsights
    }
  }

  // Real-time collaborative context sharing
  async shareContext(
    targetUsers: string[],
    contextId: string,
    permissions: {
      read: boolean
      write: boolean
      suggest: boolean
    }
  ): Promise<void> {
    if (!this.ws) return

    this.ws.send(JSON.stringify({
      type: 'share_context',
      targetUsers,
      contextId,
      permissions,
      timestamp: Date.now()
    }))
  }

  // Private helper methods
  private async generateEmbedding(text: string): Promise<number[]> {
    // Simple text embedding using character frequency and TF-IDF like approach
    // In production, this would use a proper embedding model like OpenAI embeddings
    
    const words = text.toLowerCase().split(/\s+/)
    const wordFreq: Map<string, number> = new Map()
    
    words.forEach(word => {
      wordFreq.set(word, (wordFreq.get(word) || 0) + 1)
    })

    // Generate a 384-dimensional embedding vector
    const embedding = new Array(384).fill(0)
    
    words.forEach((word, index) => {
      const hash = this.simpleHash(word)
      for (let i = 0; i < 10; i++) {
        const pos = (hash + i) % 384
        embedding[pos] += (wordFreq.get(word) || 0) / words.length
      }
    })

    // Normalize the vector
    const magnitude = Math.sqrt(embedding.reduce((sum, val) => sum + val * val, 0))
    return embedding.map(val => magnitude > 0 ? val / magnitude : 0)
  }

  private cosineSimilarity(a: number[], b: number[]): number {
    if (a.length !== b.length) return 0
    
    let dotProduct = 0
    let normA = 0
    let normB = 0
    
    for (let i = 0; i < a.length; i++) {
      dotProduct += a[i] * b[i]
      normA += a[i] * a[i]
      normB += b[i] * b[i]
    }
    
    const magnitude = Math.sqrt(normA) * Math.sqrt(normB)
    return magnitude > 0 ? dotProduct / magnitude : 0
  }

  private calculateContextualBoost(queryContext: string[], embeddingContext: string[]): number {
    if (queryContext.length === 0 || embeddingContext.length === 0) return 0
    
    const intersection = queryContext.filter(ctx => embeddingContext.includes(ctx))
    return intersection.length / Math.max(queryContext.length, embeddingContext.length)
  }

  private calculateRecencyBoost(timestamp: number): number {
    const ageInHours = (Date.now() - timestamp) / (1000 * 60 * 60)
    return Math.exp(-ageInHours / 24) // Exponential decay over 24 hours
  }

  private adaptThreshold(query: string, similarities: any[]): number {
    const queryComplexity = query.split(/\s+/).length
    const avgSimilarity = similarities.reduce((sum, s) => sum + s.similarity, 0) / similarities.length
    
    // Lower threshold for complex queries, higher for simple ones
    let adaptedThreshold = this.config.similarityThreshold
    
    if (queryComplexity > 20) {
      adaptedThreshold *= 0.8 // More lenient for complex queries
    } else if (queryComplexity < 5) {
      adaptedThreshold *= 1.2 // More strict for simple queries
    }
    
    // Adjust based on result quality
    if (avgSimilarity < 0.5) {
      adaptedThreshold *= 0.7 // More lenient if results are generally poor
    }
    
    return Math.max(0.3, Math.min(0.9, adaptedThreshold))
  }

  private async extractRelevantChunks(query: string, content: string): Promise<string[]> {
    const sentences = content.split(/[.!?]+/).filter(s => s.trim().length > 0)
    const queryWords = query.toLowerCase().split(/\s+/)
    
    const scored = sentences.map(sentence => {
      const sentenceWords = sentence.toLowerCase().split(/\s+/)
      const overlap = queryWords.filter(word => sentenceWords.includes(word)).length
      const score = overlap / Math.max(queryWords.length, sentenceWords.length)
      
      return { sentence: sentence.trim(), score }
    })
    
    return scored
      .filter(item => item.score > 0.1)
      .sort((a, b) => b.score - a.score)
      .slice(0, 3)
      .map(item => item.sentence)
  }

  private async findRelatedContent(results: SemanticSearchResult[]): Promise<SemanticSearchResult[]> {
    const relatedIds = new Set<string>()
    
    // Find content related through context graph
    results.forEach(result => {
      const related = this.contextGraph.get(result.id)
      if (related) {
        related.forEach(id => relatedIds.add(id))
      }
    })
    
    const relatedResults: SemanticSearchResult[] = []
    
    for (const id of relatedIds) {
      if (results.some(r => r.id === id)) continue // Skip duplicates
      
      const embedding = this.embeddings.get(id)
      if (embedding) {
        relatedResults.push({
          id,
          content: embedding.metadata.content,
          similarity: 0.6, // Fixed similarity for related content
          type: embedding.metadata.type,
          context: embedding.metadata.context,
          relevantChunks: []
        })
      }
    }
    
    return relatedResults.slice(0, 3) // Limit related results
  }

  private generateActionSuggestions(
    message: string,
    relatedContent: SemanticSearchResult[],
    userPreferences: any
  ): string[] {
    const suggestions = new Set<string>()
    
    // Content-based suggestions
    if (message.includes('code') || message.includes('function')) {
      suggestions.add('Run Code')
      suggestions.add('Explain Code')
      suggestions.add('Debug')
    }
    
    if (message.includes('data') || message.includes('analyze')) {
      suggestions.add('Visualize Data')
      suggestions.add('Export Data')
      suggestions.add('Create Chart')
    }
    
    if (message.includes('help') || message.includes('how')) {
      suggestions.add('Show Tutorial')
      suggestions.add('Find Documentation')
      suggestions.add('Get Examples')
    }
    
    // Related content suggestions
    if (relatedContent.some(c => c.type === 'code')) {
      suggestions.add('View Related Code')
    }
    
    if (relatedContent.some(c => c.type === 'document')) {
      suggestions.add('Read Documentation')
    }
    
    return Array.from(suggestions).slice(0, 5)
  }

  private generateContextualInsights(
    message: string,
    relatedContent: SemanticSearchResult[],
    conversationHistory: string[]
  ): string[] {
    const insights: string[] = []
    
    // Pattern recognition in conversation
    const recentTopics = this.extractTopicsFromHistory(conversationHistory)
    if (recentTopics.length > 0) {
      insights.push(`Related to recent discussion about: ${recentTopics.join(', ')}`)
    }
    
    // Content complexity analysis
    if (relatedContent.length > 5) {
      insights.push(`Found ${relatedContent.length} related resources`)
    }
    
    // Code context insights
    const codeContent = relatedContent.filter(c => c.type === 'code')
    if (codeContent.length > 0) {
      insights.push(`${codeContent.length} code examples available`)
    }
    
    return insights
  }

  private extractTopicsFromHistory(history: string[]): string[] {
    const topicKeywords = ['programming', 'data', 'analysis', 'algorithm', 'function', 'code', 'debug']
    const foundTopics = new Set<string>()
    
    history.forEach(message => {
      topicKeywords.forEach(topic => {
        if (message.toLowerCase().includes(topic)) {
          foundTopics.add(topic)
        }
      })
    })
    
    return Array.from(foundTopics)
  }

  private updateContextGraph(id: string, context: string[]): void {
    this.contextGraph.set(id, new Set(context))
    
    // Create bidirectional links
    context.forEach(ctx => {
      const existing = this.contextGraph.get(ctx) || new Set()
      existing.add(id)
      this.contextGraph.set(ctx, existing)
    })
  }

  private generateCacheKey(query: string, context: string[], options: any): string {
    return `${query}|${context.join(',')}|${JSON.stringify(options)}`
  }

  private generateId(): string {
    return Date.now().toString(36) + Math.random().toString(36).substr(2)
  }

  private simpleHash(str: string): number {
    let hash = 0
    for (let i = 0; i < str.length; i++) {
      const char = str.charCodeAt(i)
      hash = ((hash << 5) - hash) + char
      hash = hash & hash // Convert to 32-bit integer
    }
    return Math.abs(hash)
  }

  private async loadExistingEmbeddings(): Promise<void> {
    // Load from localStorage or IndexedDB
    const stored = localStorage.getItem('contextual_embeddings')
    if (stored) {
      try {
        const data = JSON.parse(stored)
        data.forEach((embedding: VectorEmbedding) => {
          this.embeddings.set(embedding.id, embedding)
        })
      } catch (error) {
        console.warn('Failed to load stored embeddings:', error)
      }
    }
  }

  private setupRealTimeSync(): void {
    if (!this.ws) return

    this.ws.addEventListener('message', (event) => {
      try {
        const data = JSON.parse(event.data)
        
        switch (data.type) {
          case 'embedding_update':
            this.embeddings.set(data.embedding.id, data.embedding)
            break
          case 'context_shared':
            this.handleSharedContext(data)
            break
          case 'collaborative_insight':
            this.handleCollaborativeInsight(data)
            break
        }
      } catch (error) {
        console.warn('Failed to handle real-time sync message:', error)
      }
    })

    // Periodically save to localStorage
    setInterval(() => {
      const data = Array.from(this.embeddings.values())
      localStorage.setItem('contextual_embeddings', JSON.stringify(data))
    }, 30000) // Save every 30 seconds
  }

  private handleSharedContext(data: any): void {
    // Handle shared context from other users
    console.log('Received shared context:', data)
  }

  private handleCollaborativeInsight(data: any): void {
    // Handle collaborative insights
    console.log('Received collaborative insight:', data)
  }
}

export default ContextualRetrievalSystem