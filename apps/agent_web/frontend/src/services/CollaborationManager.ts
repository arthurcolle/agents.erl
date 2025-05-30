interface CollaborationSession {
  id: string
  participants: Participant[]
  sharedState: SharedState
  permissions: PermissionSet
  createdAt: number
  lastActivity: number
}

interface Participant {
  id: string
  name: string
  avatar?: string
  role: 'owner' | 'collaborator' | 'viewer'
  cursor?: { x: number; y: number }
  selection?: Selection
  isActive: boolean
  lastSeen: number
}

interface SharedState {
  messages: SharedMessage[]
  widgets: SharedWidget[]
  ui: UIState
  annotations: Annotation[]
  documents: SharedDocument[]
}

interface SharedMessage {
  id: string
  content: string
  sender: string
  timestamp: number
  edits: Edit[]
  reactions: Reaction[]
  mentions: string[]
}

interface SharedWidget {
  id: string
  type: string
  data: any
  position: { x: number; y: number }
  size: { width: number; height: number }
  permissions: string[]
  collaborativeState: any
}

interface Edit {
  id: string
  userId: string
  timestamp: number
  type: 'insert' | 'delete' | 'modify'
  position: number
  content: string
  oldContent?: string
}

interface Reaction {
  userId: string
  emoji: string
  timestamp: number
}

interface Selection {
  messageId: string
  start: number
  end: number
  type: 'text' | 'code' | 'widget'
}

interface Annotation {
  id: string
  userId: string
  position: { x: number; y: number }
  content: string
  type: 'comment' | 'highlight' | 'sticky-note'
  timestamp: number
  resolved: boolean
}

interface SharedDocument {
  id: string
  name: string
  content: string
  type: 'markdown' | 'code' | 'data'
  language?: string
  version: number
  lastEdit: number
  collaborativeEdits: Edit[]
}

interface PermissionSet {
  canEdit: boolean
  canInvite: boolean
  canManageWidgets: boolean
  canAnnotate: boolean
  canShare: boolean
  canExport: boolean
}

interface UIState {
  layout: string
  theme: string
  zoom: number
  viewportCenter: { x: number; y: number }
  activePanel: string
  customSettings: Record<string, any>
}

interface PresenceUpdate {
  userId: string
  cursor?: { x: number; y: number }
  selection?: Selection
  activity: 'typing' | 'idle' | 'active' | 'editing'
  data?: any
}

interface ConflictResolution {
  strategy: 'last-write-wins' | 'operational-transform' | 'manual-merge'
  conflictId: string
  originalValue: any
  conflictingValues: Array<{ userId: string; value: any; timestamp: number }>
}

export class CollaborationManager {
  private ws: WebSocket | null = null
  private sessionId: string | null = null
  private currentUser: Participant | null = null
  private session: CollaborationSession | null = null
  private presenceInterval: any = null
  private conflictQueue: ConflictResolution[] = []
  private eventCallbacks: Map<string, Function[]> = new Map()

  constructor() {
    this.initializeEventHandlers()
  }

  // Initialize collaboration session
  async createSession(
    initialState?: Partial<SharedState>,
    permissions?: Partial<PermissionSet>
  ): Promise<string> {
    const sessionId = this.generateSessionId()
    
    const session: CollaborationSession = {
      id: sessionId,
      participants: [this.currentUser!],
      sharedState: {
        messages: [],
        widgets: [],
        ui: {
          layout: 'standard',
          theme: 'light',
          zoom: 1,
          viewportCenter: { x: 0, y: 0 },
          activePanel: 'chat',
          customSettings: {}
        },
        annotations: [],
        documents: [],
        ...initialState
      },
      permissions: {
        canEdit: true,
        canInvite: true,
        canManageWidgets: true,
        canAnnotate: true,
        canShare: true,
        canExport: true,
        ...permissions
      },
      createdAt: Date.now(),
      lastActivity: Date.now()
    }

    this.session = session
    this.sessionId = sessionId

    if (this.ws) {
      this.ws.send(JSON.stringify({
        type: 'create_collaboration_session',
        sessionId,
        session
      }))
    }

    this.startPresenceTracking()
    return sessionId
  }

  // Join existing session
  async joinSession(sessionId: string, user: Omit<Participant, 'isActive' | 'lastSeen'>): Promise<void> {
    this.sessionId = sessionId
    this.currentUser = {
      ...user,
      isActive: true,
      lastSeen: Date.now()
    }

    if (this.ws) {
      this.ws.send(JSON.stringify({
        type: 'join_collaboration_session',
        sessionId,
        user: this.currentUser
      }))
    }

    this.startPresenceTracking()
  }

  // Initialize WebSocket connection
  initializeWebSocket(ws: WebSocket): void {
    this.ws = ws
    
    this.ws.addEventListener('message', (event) => {
      try {
        const data = JSON.parse(event.data)
        this.handleWebSocketMessage(data)
      } catch (error) {
        console.error('Failed to parse collaboration message:', error)
      }
    })

    this.ws.addEventListener('close', () => {
      this.stopPresenceTracking()
    })
  }

  // Share content with operational transformation
  async shareMessage(content: string, messageId?: string): Promise<void> {
    if (!this.session || !this.currentUser) return

    const message: SharedMessage = {
      id: messageId || this.generateId(),
      content,
      sender: this.currentUser.id,
      timestamp: Date.now(),
      edits: [],
      reactions: [],
      mentions: this.extractMentions(content)
    }

    // Apply operational transformation for concurrent edits
    if (messageId && this.session.sharedState.messages.find(m => m.id === messageId)) {
      const edit: Edit = {
        id: this.generateId(),
        userId: this.currentUser.id,
        timestamp: Date.now(),
        type: 'modify',
        position: 0,
        content,
        oldContent: this.session.sharedState.messages.find(m => m.id === messageId)?.content
      }
      
      await this.applyOperationalTransform(messageId, edit)
    } else {
      this.session.sharedState.messages.push(message)
      this.broadcastUpdate('message_shared', message)
    }

    this.session.lastActivity = Date.now()
  }

  // Real-time widget collaboration
  async shareWidget(widget: SharedWidget): Promise<void> {
    if (!this.session) return

    const existingIndex = this.session.sharedState.widgets.findIndex(w => w.id === widget.id)
    
    if (existingIndex >= 0) {
      // Check for conflicts
      const existing = this.session.sharedState.widgets[existingIndex]
      if (this.hasConflict(existing, widget)) {
        await this.resolveConflict('widget_conflict', widget.id, existing, widget)
        return
      }
      
      this.session.sharedState.widgets[existingIndex] = widget
    } else {
      this.session.sharedState.widgets.push(widget)
    }

    this.broadcastUpdate('widget_updated', widget)
  }

  // Collaborative annotations
  async addAnnotation(
    position: { x: number; y: number },
    content: string,
    type: 'comment' | 'highlight' | 'sticky-note' = 'comment'
  ): Promise<string> {
    if (!this.session || !this.currentUser) return ''

    const annotation: Annotation = {
      id: this.generateId(),
      userId: this.currentUser.id,
      position,
      content,
      type,
      timestamp: Date.now(),
      resolved: false
    }

    this.session.sharedState.annotations.push(annotation)
    this.broadcastUpdate('annotation_added', annotation)
    
    return annotation.id
  }

  // Real-time presence tracking
  updatePresence(update: Partial<PresenceUpdate>): void {
    if (!this.currentUser || !this.ws) return

    const presenceData: PresenceUpdate = {
      userId: this.currentUser.id,
      activity: 'active',
      ...update
    }

    this.currentUser.cursor = presenceData.cursor
    this.currentUser.selection = presenceData.selection
    this.currentUser.lastSeen = Date.now()

    this.ws.send(JSON.stringify({
      type: 'presence_update',
      sessionId: this.sessionId,
      presence: presenceData
    }))
  }

  // Conflict resolution
  private async resolveConflict(
    type: string,
    resourceId: string,
    originalValue: any,
    conflictingValue: any
  ): Promise<void> {
    const conflict: ConflictResolution = {
      strategy: 'operational-transform', // Default strategy
      conflictId: this.generateId(),
      originalValue,
      conflictingValues: [
        {
          userId: this.currentUser!.id,
          value: conflictingValue,
          timestamp: Date.now()
        }
      ]
    }

    this.conflictQueue.push(conflict)
    
    // Notify other participants about the conflict
    this.broadcastUpdate('conflict_detected', {
      type,
      resourceId,
      conflictId: conflict.conflictId
    })

    // Attempt automatic resolution
    switch (conflict.strategy) {
      case 'operational-transform':
        await this.applyOperationalTransform(resourceId, conflictingValue)
        break
      case 'last-write-wins':
        this.applyLastWriteWins(resourceId, conflictingValue)
        break
      case 'manual-merge':
        this.promptManualMerge(conflict)
        break
    }
  }

  // Operational Transform implementation
  private async applyOperationalTransform(resourceId: string, operation: any): Promise<void> {
    // Simplified OT implementation
    // In production, use a proper OT library like ShareJS or Yjs
    
    if (!this.session) return

    const resource = this.findResource(resourceId)
    if (!resource) return

    // Apply transformation based on concurrent operations
    const transformedOperation = this.transformOperation(operation, resource)
    
    // Apply the transformed operation
    this.applyOperation(resourceId, transformedOperation)
    
    // Broadcast the transformed operation
    this.broadcastUpdate('operation_applied', {
      resourceId,
      operation: transformedOperation
    })
  }

  // Document collaboration
  async shareDocument(document: SharedDocument): Promise<void> {
    if (!this.session) return

    const existingIndex = this.session.sharedState.documents.findIndex(d => d.id === document.id)
    
    if (existingIndex >= 0) {
      this.session.sharedState.documents[existingIndex] = document
    } else {
      this.session.sharedState.documents.push(document)
    }

    this.broadcastUpdate('document_shared', document)
  }

  async editDocument(
    documentId: string,
    edit: Omit<Edit, 'id' | 'userId' | 'timestamp'>
  ): Promise<void> {
    if (!this.session || !this.currentUser) return

    const document = this.session.sharedState.documents.find(d => d.id === documentId)
    if (!document) return

    const fullEdit: Edit = {
      id: this.generateId(),
      userId: this.currentUser.id,
      timestamp: Date.now(),
      ...edit
    }

    document.collaborativeEdits.push(fullEdit)
    document.lastEdit = Date.now()
    document.version++

    // Apply the edit
    this.applyDocumentEdit(document, fullEdit)
    
    this.broadcastUpdate('document_edited', {
      documentId,
      edit: fullEdit,
      newContent: document.content
    })
  }

  // UI synchronization
  syncUIState(uiState: Partial<UIState>): void {
    if (!this.session) return

    this.session.sharedState.ui = {
      ...this.session.sharedState.ui,
      ...uiState
    }

    this.broadcastUpdate('ui_synced', this.session.sharedState.ui)
  }

  // Session management
  async inviteParticipant(email: string, role: 'collaborator' | 'viewer' = 'collaborator'): Promise<void> {
    if (!this.session || !this.session.permissions.canInvite) return

    this.broadcastUpdate('participant_invited', {
      sessionId: this.sessionId,
      email,
      role,
      invitedBy: this.currentUser!.id
    })
  }

  leaveSession(): void {
    if (this.ws && this.sessionId) {
      this.ws.send(JSON.stringify({
        type: 'leave_collaboration_session',
        sessionId: this.sessionId,
        userId: this.currentUser?.id
      }))
    }

    this.stopPresenceTracking()
    this.session = null
    this.sessionId = null
    this.currentUser = null
  }

  // Event system
  on(event: string, callback: Function): void {
    if (!this.eventCallbacks.has(event)) {
      this.eventCallbacks.set(event, [])
    }
    this.eventCallbacks.get(event)!.push(callback)
  }

  off(event: string, callback: Function): void {
    const callbacks = this.eventCallbacks.get(event)
    if (callbacks) {
      const index = callbacks.indexOf(callback)
      if (index > -1) {
        callbacks.splice(index, 1)
      }
    }
  }

  private emit(event: string, data: any): void {
    const callbacks = this.eventCallbacks.get(event)
    if (callbacks) {
      callbacks.forEach(callback => callback(data))
    }
  }

  // Helper methods
  private handleWebSocketMessage(data: any): void {
    switch (data.type) {
      case 'collaboration_session_joined':
        this.session = data.session
        this.emit('session_joined', data.session)
        break
      
      case 'participant_joined':
        if (this.session) {
          this.session.participants.push(data.participant)
          this.emit('participant_joined', data.participant)
        }
        break

      case 'participant_left':
        if (this.session) {
          this.session.participants = this.session.participants.filter(
            p => p.id !== data.participantId
          )
          this.emit('participant_left', data.participantId)
        }
        break

      case 'presence_updated':
        this.updateParticipantPresence(data.presence)
        this.emit('presence_updated', data.presence)
        break

      case 'shared_state_updated':
        this.handleSharedStateUpdate(data)
        break

      case 'conflict_resolution':
        this.handleConflictResolution(data.resolution)
        break

      default:
        console.log('Unhandled collaboration message:', data.type)
    }
  }

  private updateParticipantPresence(presence: PresenceUpdate): void {
    if (!this.session) return

    const participant = this.session.participants.find(p => p.id === presence.userId)
    if (participant) {
      participant.cursor = presence.cursor
      participant.selection = presence.selection
      participant.lastSeen = Date.now()
      participant.isActive = presence.activity !== 'idle'
    }
  }

  private handleSharedStateUpdate(data: any): void {
    if (!this.session) return

    switch (data.updateType) {
      case 'message_shared':
        this.session.sharedState.messages.push(data.update)
        this.emit('message_received', data.update)
        break

      case 'widget_updated':
        const widgetIndex = this.session.sharedState.widgets.findIndex(w => w.id === data.update.id)
        if (widgetIndex >= 0) {
          this.session.sharedState.widgets[widgetIndex] = data.update
        } else {
          this.session.sharedState.widgets.push(data.update)
        }
        this.emit('widget_updated', data.update)
        break

      case 'annotation_added':
        this.session.sharedState.annotations.push(data.update)
        this.emit('annotation_added', data.update)
        break

      case 'ui_synced':
        this.session.sharedState.ui = data.update
        this.emit('ui_synced', data.update)
        break
    }
  }

  private handleConflictResolution(resolution: any): void {
    const conflictIndex = this.conflictQueue.findIndex(c => c.conflictId === resolution.conflictId)
    if (conflictIndex >= 0) {
      this.conflictQueue.splice(conflictIndex, 1)
      this.emit('conflict_resolved', resolution)
    }
  }

  private broadcastUpdate(type: string, data: any): void {
    if (!this.ws || !this.sessionId) return

    this.ws.send(JSON.stringify({
      type: 'collaboration_update',
      sessionId: this.sessionId,
      updateType: type,
      update: data,
      userId: this.currentUser?.id
    }))
  }

  private startPresenceTracking(): void {
    this.presenceInterval = setInterval(() => {
      this.updatePresence({ activity: 'active' })
    }, 5000) // Update every 5 seconds
  }

  private stopPresenceTracking(): void {
    if (this.presenceInterval) {
      clearInterval(this.presenceInterval)
      this.presenceInterval = null
    }
  }

  private initializeEventHandlers(): void {
    // Mouse movement tracking for cursor sharing
    document.addEventListener('mousemove', (e) => {
      if (this.session) {
        this.updatePresence({
          cursor: { x: e.clientX, y: e.clientY },
          activity: 'active'
        })
      }
    })

    // Text selection tracking
    document.addEventListener('selectionchange', () => {
      const selection = window.getSelection()
      if (selection && selection.rangeCount > 0 && this.session) {
        const range = selection.getRangeAt(0)
        // Simplified selection tracking
        this.updatePresence({
          selection: {
            messageId: 'current',
            start: range.startOffset,
            end: range.endOffset,
            type: 'text'
          },
          activity: 'active'
        })
      }
    })
  }

  private generateSessionId(): string {
    return `session_${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
  }

  private generateId(): string {
    return `${Date.now()}_${Math.random().toString(36).substr(2, 9)}`
  }

  private extractMentions(content: string): string[] {
    const mentions = content.match(/@\w+/g) || []
    return mentions.map(mention => mention.slice(1))
  }

  private hasConflict(existing: any, incoming: any): boolean {
    // Simple conflict detection based on timestamps
    return existing.lastModified && incoming.lastModified && 
           Math.abs(existing.lastModified - incoming.lastModified) < 1000
  }

  private findResource(resourceId: string): any {
    if (!this.session) return null

    // Search in different resource types
    return this.session.sharedState.messages.find(m => m.id === resourceId) ||
           this.session.sharedState.widgets.find(w => w.id === resourceId) ||
           this.session.sharedState.documents.find(d => d.id === resourceId)
  }

  private transformOperation(operation: any, resource: any): any {
    // Simplified transformation - in production use proper OT algorithms
    return operation
  }

  private applyOperation(resourceId: string, operation: any): void {
    // Apply operation to the resource
    const resource = this.findResource(resourceId)
    if (resource) {
      // Apply the operation based on its type
      switch (operation.type) {
        case 'insert':
          // Handle insertion
          break
        case 'delete':
          // Handle deletion
          break
        case 'modify':
          // Handle modification
          break
      }
    }
  }

  private applyLastWriteWins(resourceId: string, value: any): void {
    const resource = this.findResource(resourceId)
    if (resource) {
      Object.assign(resource, value)
    }
  }

  private promptManualMerge(conflict: ConflictResolution): void {
    this.emit('manual_merge_required', conflict)
  }

  private applyDocumentEdit(document: SharedDocument, edit: Edit): void {
    switch (edit.type) {
      case 'insert':
        document.content = document.content.slice(0, edit.position) + 
                          edit.content + 
                          document.content.slice(edit.position)
        break
      case 'delete':
        document.content = document.content.slice(0, edit.position) + 
                          document.content.slice(edit.position + edit.content.length)
        break
      case 'modify':
        if (edit.oldContent) {
          document.content = document.content.replace(edit.oldContent, edit.content)
        }
        break
    }
  }

  // Getters
  get isInSession(): boolean {
    return this.session !== null
  }

  get sessionData(): CollaborationSession | null {
    return this.session
  }

  get participants(): Participant[] {
    return this.session?.participants || []
  }

  get currentUserData(): Participant | null {
    return this.currentUser
  }
}

export default CollaborationManager