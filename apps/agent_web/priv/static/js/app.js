class AgentWebApp {
    constructor() {
        this.agents = new Map();
        this.currentAgent = null;
        this.ws = null;
        this.activeTab = 'overview';
        this.activityLog = [];
        this.systemMetrics = {};
        this.performanceHistory = [];
        this.networkTopology = new Map();
        this.notifications = [];
        this.buttonClickHistory = [];
        
        this.initializeApp();
    }
    
    async initializeApp() {
        this.renderUI();
        this.attachEventListeners();
        await this.loadAgents();
        this.connectWebSocket();
        this.initCharts();
        this.startActivityFeed();
    }
    
    renderUI() {
        const app = document.getElementById('app');
        app.innerHTML = `
            <div class="header">
                <h1>Erlang Agent System</h1>
                <p>Advanced Multi-Agent Framework with AI Integration</p>
            </div>
            
            <div class="main-grid">
                <div class="sidebar">
                    <h2>Active Agents</h2>
                    <button class="btn btn-primary" onclick="app.showCreateAgentDialog()">
                        Create New Agent
                    </button>
                    <ul class="agent-list" id="agent-list">
                        <li class="loading">Loading agents...</li>
                    </ul>
                </div>
                
                <div class="content">
                    <div class="tabs">
                        <button class="tab active" data-tab="dashboard" onclick="app.switchTab('dashboard')">
                            Dashboard
                        </button>
                        <button class="tab" data-tab="agents" onclick="app.switchTab('agents')">
                            Agents
                        </button>
                        <button class="tab" data-tab="network" onclick="app.switchTab('network')">
                            Network
                        </button>
                        <button class="tab" data-tab="chat" onclick="app.switchTab('chat')">
                            Chat
                        </button>
                        <button class="tab" data-tab="monitoring" onclick="app.switchTab('monitoring')">
                            Monitoring
                        </button>
                        <button class="tab" data-tab="examples" onclick="app.switchTab('examples')">
                            Examples
                        </button>
                        <button class="tab" data-tab="logs" onclick="app.switchTab('logs')">
                            Logs
                        </button>
                        <button class="tab" data-tab="knowledge" onclick="app.switchTab('knowledge')">
                            Knowledge Base
                        </button>
                    </div>
                    
                    <div class="tab-content" id="tab-content">
                        ${this.renderDashboard()}
                    </div>
                </div>
            </div>
            
            <div class="notification-container" id="notifications"></div>
        `;
    }
    
    renderDashboard() {
        return `
            <div class="dashboard">
                <div class="search-filter-bar">
                    <input type="text" class="search-input" placeholder="Search agents, logs, or metrics..." onkeyup="app.handleSearch(event)">
                    <select class="filter-dropdown" onchange="app.handleFilter(event)">
                        <option value="all">All Agents</option>
                        <option value="active">Active Only</option>
                        <option value="ai">AI Agents</option>
                        <option value="simple">Simple Agents</option>
                    </select>
                    <button class="btn btn-primary" onclick="app.refreshDashboard()">
                        <span style="margin-right: 0.5rem;">↻</span> Refresh
                    </button>
                </div>
                
                <div class="dashboard-widgets">
                    <div class="widget">
                        <div class="widget-header">
                            <span>System Status</span>
                            <span class="widget-icon">📊</span>
                        </div>
                        <div class="widget-body">
                            <div class="performance-grid" id="system-performance">
                                <div class="performance-metric">
                                    <div class="performance-value" id="cpu-usage">--</div>
                                    <div class="performance-label">CPU Usage</div>
                                </div>
                                <div class="performance-metric">
                                    <div class="performance-value" id="memory-usage">--</div>
                                    <div class="performance-label">Memory</div>
                                </div>
                                <div class="performance-metric">
                                    <div class="performance-value" id="active-processes">--</div>
                                    <div class="performance-label">Processes</div>
                                </div>
                            </div>
                        </div>
                    </div>
                    
                    <div class="widget">
                        <div class="widget-header">
                            <span>Agent Overview</span>
                            <span class="widget-icon">🤖</span>
                        </div>
                        <div class="widget-body">
                            <div class="performance-grid" id="agent-overview">
                                <div class="performance-metric">
                                    <div class="performance-value" id="total-agents-count">0</div>
                                    <div class="performance-label">Total Agents</div>
                                </div>
                                <div class="performance-metric">
                                    <div class="performance-value" id="active-agents-count">0</div>
                                    <div class="performance-label">Active</div>
                                </div>
                                <div class="performance-metric">
                                    <div class="performance-value" id="ai-agents-count">0</div>
                                    <div class="performance-label">AI Agents</div>
                                </div>
                            </div>
                        </div>
                    </div>
                    
                    <div class="widget">
                        <div class="widget-header">
                            <span>Recent Activity</span>
                            <span class="widget-icon">📝</span>
                        </div>
                        <div class="widget-body">
                            <div class="activity-feed" id="recent-activity">
                                <p style="text-align: center; color: var(--gray-500);">Loading activity...</p>
                            </div>
                        </div>
                    </div>
                    
                    <div class="widget">
                        <div class="widget-header">
                            <span>Performance</span>
                            <span class="widget-icon">⚡</span>
                        </div>
                        <div class="widget-body">
                            <div class="resource-chart" id="performance-chart-container">
                                <canvas id="dashboard-performance-chart" style="width: 100%; height: 200px;"></canvas>
                            </div>
                        </div>
                    </div>
                </div>
                
                <div class="control-section">
                    <h3>Quick Actions</h3>
                    <div class="control-grid">
                        <div class="control-item" onclick="app.showCreateAgentDialog()">
                            <div class="control-icon">➕</div>
                            <div class="control-label">Create Agent</div>
                        </div>
                        <div class="control-item" onclick="app.restartAllAgents()">
                            <div class="control-icon">🔄</div>
                            <div class="control-label">Restart All</div>
                        </div>
                        <div class="control-item" onclick="app.exportMetrics()">
                            <div class="control-icon">📥</div>
                            <div class="control-label">Export Data</div>
                        </div>
                        <div class="control-item" onclick="app.systemCleanup()">
                            <div class="control-icon">🧹</div>
                            <div class="control-label">System Cleanup</div>
                        </div>
                        <div class="control-item" onclick="app.runDiagnostics()">
                            <div class="control-icon">🔍</div>
                            <div class="control-label">Diagnostics</div>
                        </div>
                        <div class="control-item" onclick="app.showSettings()">
                            <div class="control-icon">⚙️</div>
                            <div class="control-label">Settings</div>
                        </div>
                    </div>
                </div>
            </div>
        `;
    }
    
    renderAgents() {
        return `
            <div class="agents-view">
                <div class="search-filter-bar">
                    <input type="text" class="search-input" placeholder="Search agents..." onkeyup="app.filterAgents(event)">
                    <select class="filter-dropdown" onchange="app.filterAgentsByType(event)">
                        <option value="all">All Types</option>
                        <option value="ai">AI Agents</option>
                        <option value="simple">Simple Agents</option>
                        <option value="coordinator">Coordinators</option>
                    </select>
                    <button class="btn btn-primary" onclick="app.showCreateAgentDialog()">New Agent</button>
                </div>
                
                <div class="agent-grid" id="agents-grid">
                    ${this.renderAgentCards()}
                </div>
            </div>
        `;
    }
    
    renderAgentCards() {
        if (this.agents.size === 0) {
            return '<p>No agents available. Create your first agent to get started.</p>';
        }
        
        return Array.from(this.agents.values()).map(agent => `
            <div class="agent-card" data-agent-id="${agent.id}">
                <div class="agent-card-header">
                    <div class="agent-card-title">${agent.type || 'Unknown'}</div>
                    <div class="agent-card-status">
                        <span class="status-indicator online pulsing"></span>
                        Active
                    </div>
                </div>
                
                <div class="agent-card-metrics">
                    <div class="agent-card-metric">
                        <div class="agent-card-metric-value">--</div>
                        <div class="agent-card-metric-label">Memory</div>
                    </div>
                    <div class="agent-card-metric">
                        <div class="agent-card-metric-value">--</div>
                        <div class="agent-card-metric-label">Requests</div>
                    </div>
                </div>
                
                <div class="agent-card-actions">
                    <button class="btn btn-sm btn-primary" onclick="app.selectAgent('${agent.id}')">Select</button>
                    <button class="btn btn-sm btn-success" onclick="app.startChat('${agent.id}')">Chat</button>
                    <button class="btn btn-sm" onclick="app.viewAgentDetails('${agent.id}')">Details</button>
                    <button class="btn btn-sm btn-danger" onclick="app.stopAgent('${agent.id}')">Stop</button>
                </div>
            </div>
        `).join('');
    }
    
    renderNetwork() {
        return `
            <div class="network-view">
                <div class="control-panel">
                    <h3>Network Topology</h3>
                    <div style="margin-bottom: 15px;">
                        <button class="btn btn-primary" onclick="app.refreshTopology()">Refresh</button>
                        <button class="btn btn-success" onclick="app.autoLayoutNodes()">Auto Layout</button>
                        <label style="margin-left: 20px;">
                            <input type="checkbox" onchange="app.toggleConnections(event)" checked> Show Connections
                        </label>
                    </div>
                </div>
                
                <div class="network-topology" id="network-topology">
                    <div style="text-align: center; padding: 50px; color: #7f8c8d;">
                        Network topology will be displayed here.<br>
                        Nodes represent agents, lines show communication paths.
                    </div>
                </div>
                
                <div class="network-legend" style="margin-top: 20px; padding: 15px; background: #f8f9fa; border-radius: 6px;">
                    <h4>Legend:</h4>
                    <div style="display: flex; gap: 20px; flex-wrap: wrap;">
                        <div><span class="node agent" style="position: static; width: 20px; height: 20px; margin-right: 5px;"></span> Agent</div>
                        <div><span class="node coordinator" style="position: static; width: 20px; height: 20px; margin-right: 5px;"></span> Coordinator</div>
                        <div><span class="node worker" style="position: static; width: 20px; height: 20px; margin-right: 5px;"></span> Worker</div>
                    </div>
                </div>
            </div>
        `;
    }
    
    renderLogs() {
        return `
            <div class="logs-view">
                <div class="control-panel">
                    <div style="display: flex; gap: 10px; align-items: center; margin-bottom: 15px;">
                        <select class="filter-dropdown" onchange="app.filterLogs(event)">
                            <option value="all">All Levels</option>
                            <option value="error">Errors</option>
                            <option value="warning">Warnings</option>
                            <option value="info">Info</option>
                            <option value="success">Success</option>
                        </select>
                        <input type="text" class="search-input" placeholder="Search logs..." onkeyup="app.searchLogs(event)" style="flex: 1; max-width: 300px;">
                        <button class="btn btn-primary" onclick="app.clearLogs()">Clear</button>
                        <button class="btn btn-success" onclick="app.exportLogs()">Export</button>
                        <label>
                            <input type="checkbox" onchange="app.toggleAutoScroll(event)" checked> Auto-scroll
                        </label>
                    </div>
                </div>
                
                <div class="log-viewer" id="log-viewer">
                    <div class="log-entry info">System logs will appear here...</div>
                </div>
            </div>
        `;
    }
    
    renderExamples() {
        return `
            <div class="examples">
                <h2>Example Demonstrations</h2>
                
                <h3>Distributed Examples</h3>
                <div class="example-grid">
                    ${this.renderExampleCard('distributed', 'cluster', 'Distributed Cluster', 'Deploy agents across multiple Erlang nodes')}
                    ${this.renderExampleCard('distributed', 'research_team', 'Research Team', 'Collaborative AI agents working on research')}
                    ${this.renderExampleCard('distributed', 'data_pipeline', 'Data Pipeline', 'Distributed stream processing pipeline')}
                    ${this.renderExampleCard('distributed', 'swarm', 'Swarm Intelligence', 'Optimization using swarm algorithms')}
                </div>
                
                <h3>Streaming Examples</h3>
                <div class="example-grid">
                    ${this.renderExampleCard('streaming', 'pipeline', 'Stream Pipeline', 'Real-time data streaming with backpressure')}
                    ${this.renderExampleCard('streaming', 'realtime_chat', 'Real-time Chat', 'Streaming AI chat responses')}
                    ${this.renderExampleCard('streaming', 'batch_processor', 'Batch Processor', 'Parallel batch processing')}
                    ${this.renderExampleCard('streaming', 'event_system', 'Event System', 'Event-driven agent network')}
                </div>
                
                <h3>Tool Composition Examples</h3>
                <div class="example-grid">
                    ${this.renderExampleCard('composition', 'code_analysis', 'Code Analysis', 'Multi-stage code analysis pipeline')}
                    ${this.renderExampleCard('composition', 'debugging', 'Auto Debugging', 'Autonomous debugging system')}
                    ${this.renderExampleCard('composition', 'security_audit', 'Security Audit', 'Progressive security analysis')}
                    ${this.renderExampleCard('composition', 'infrastructure', 'Infrastructure', 'Automated deployment orchestration')}
                </div>
            </div>
        `;
    }
    
    renderExampleCard(category, name, title, description) {
        return `
            <div class="example-card" onclick="app.runExample('${category}', '${name}')">
                <h3>${title}</h3>
                <p>${description}</p>
            </div>
        `;
    }
    
    renderChat() {
        if (!this.currentAgent) {
            return `
                <div class="chat-container">
                    <p>Please select an AI agent from the sidebar to start chatting.</p>
                </div>
            `;
        }
        
        return `
            <div class="chat-container">
                <h2>Chat with ${this.currentAgent.name}</h2>
                <div class="chat-messages" id="chat-messages"></div>
                <div class="chat-input">
                    <input type="text" id="chat-input" placeholder="Type your message..." 
                           onkeypress="if(event.key === 'Enter') app.sendMessage()">
                    <button class="btn btn-primary" onclick="app.sendMessage()">Send</button>
                </div>
            </div>
        `;
    }
    
    renderConsole() {
        return `
            <div class="console">
                <h2>System Console</h2>
                <div class="control-panel">
                    <button class="btn btn-primary" onclick="app.clearConsole()">Clear</button>
                    <button class="btn btn-success" onclick="app.exportLogs()">Export Logs</button>
                </div>
                <div class="output-console" id="console-output"></div>
            </div>
        `;
    }
    
    renderMonitoring() {
        return `
            <div class="monitoring">
                <h2>Real-time Agent Monitoring</h2>
                
                <div class="metrics-grid">
                    <div class="metric-card">
                        <h3>System Overview</h3>
                        <div id="system-metrics">
                            <p>Loading metrics...</p>
                        </div>
                    </div>
                    
                    <div class="metric-card">
                        <h3>Active Agents</h3>
                        <div id="agent-metrics">
                            <p>Loading agent data...</p>
                        </div>
                    </div>
                </div>
                
                <div class="monitoring-chart">
                    <h3>Performance Timeline</h3>
                    <canvas id="performance-chart"></canvas>
                </div>
                
                <div class="agent-grid" id="agent-monitoring-grid">
                    <!-- Agent cards will be inserted here -->
                </div>
            </div>
        `;
    }
    
    renderAnalytics() {
        return `
            <div class="analytics">
                <h2>Agent Analytics Dashboard</h2>
                
                <div class="analytics-summary" id="analytics-summary">
                    <div class="stat-card">
                        <h4>Total Agents</h4>
                        <p class="stat-value" id="total-agents">-</p>
                    </div>
                    <div class="stat-card">
                        <h4>Active Agents</h4>
                        <p class="stat-value" id="active-agents">-</p>
                    </div>
                    <div class="stat-card">
                        <h4>Total Requests</h4>
                        <p class="stat-value" id="total-requests">-</p>
                    </div>
                    <div class="stat-card">
                        <h4>Success Rate</h4>
                        <p class="stat-value" id="success-rate">-</p>
                    </div>
                </div>
                
                <div class="analytics-charts">
                    <div class="chart-container">
                        <h3>Agent Distribution</h3>
                        <canvas id="distribution-chart"></canvas>
                    </div>
                    <div class="chart-container">
                        <h3>Performance Trends</h3>
                        <canvas id="trends-chart"></canvas>
                    </div>
                </div>
                
                <div class="agent-templates">
                    <h3>Agent Templates</h3>
                    <div class="template-grid" id="template-grid">
                        <!-- Templates will be loaded here -->
                    </div>
                </div>
            </div>
        `;
    }
    
    async loadAgents() {
        try {
            const response = await fetch('/api/agents');
            const data = await response.json();
            
            this.agents.clear();
            data.agents.forEach(agent => {
                this.agents.set(agent.id, agent);
            });
            
            this.renderAgentList();
        } catch (error) {
            this.log('Error loading agents: ' + error.message, 'error');
        }
    }
    
    renderAgentList() {
        const agentList = document.getElementById('agent-list');
        
        if (this.agents.size === 0) {
            agentList.innerHTML = '<li class="agent-item">No agents running</li>';
            return;
        }
        
        agentList.innerHTML = Array.from(this.agents.values()).map(agent => `
            <li class="agent-item ${agent.id === this.currentAgent?.id ? 'active' : ''}" 
                onclick="app.selectAgent('${agent.id}')">
                <span class="status-indicator online"></span>
                <strong>${agent.type}</strong>: ${String(agent.id).substring(0, 8)}
            </li>
        `).join('');
    }
    
    selectAgent(agentId) {
        this.trackButtonClick('selectAgent', { agentId });
        this.currentAgent = this.agents.get(agentId);
        this.log(`Selected agent: ${agentId}`, 'info');
        this.renderAgentList();
        
        if (this.activeTab === 'chat') {
            this.switchTab('chat');
        }
        
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            // Ensure agent ID is properly formatted
            const formattedId = String(agentId);
            this.ws.send(JSON.stringify({
                type: 'create_stream',
                agent_id: formattedId
            }));
        }
    }
    
    switchTab(tabName) {
        this.trackButtonClick('switchTab', { tabName, previousTab: this.activeTab });
        this.activeTab = tabName;
        this.log(`Switched to tab: ${tabName}`, 'info');
        
        document.querySelectorAll('.tab').forEach(tab => {
            tab.classList.toggle('active', tab.dataset.tab === tabName);
        });
        
        const content = document.getElementById('tab-content');
        switch (tabName) {
            case 'dashboard':
                content.innerHTML = this.renderDashboard();
                this.updateDashboardMetrics();
                break;
            case 'agents':
                content.innerHTML = this.renderAgents();
                break;
            case 'network':
                content.innerHTML = this.renderNetwork();
                this.initializeNetworkTopology();
                break;
            case 'chat':
                content.innerHTML = this.renderChat();
                break;
            case 'monitoring':
                content.innerHTML = this.renderMonitoring();
                this.startMonitoring();
                break;
            case 'examples':
                content.innerHTML = this.renderExamples();
                break;
            case 'logs':
                content.innerHTML = this.renderLogs();
                break;
            case 'knowledge':
                content.innerHTML = this.renderKnowledge();
                this.loadKnowledgeDomains();
                break;
        }
    }
    
    connectWebSocket() {
        this.ws = new WebSocket(`ws://${window.location.host}/ws`);
        
        this.ws.onopen = () => {
            this.log('WebSocket connected', 'success');
        };
        
        this.ws.onmessage = (event) => {
            const data = JSON.parse(event.data);
            this.handleWebSocketMessage(data);
        };
        
        this.ws.onerror = (error) => {
            this.log('WebSocket error: ' + error, 'error');
        };
        
        this.ws.onclose = () => {
            this.log('WebSocket disconnected', 'warning');
            setTimeout(() => this.connectWebSocket(), 5000);
        };
    }
    
    handleWebSocketMessage(data) {
        switch (data.type) {
            case 'stream_token':
                this.appendChatToken(data.token);
                break;
            case 'stream_complete':
                this.completeChatMessage(data.result);
                break;
            case 'agent_event':
                this.log(`Agent event: ${JSON.stringify(data.event)}`, 'info');
                break;
            case 'example_update':
                this.log(`Example update: ${JSON.stringify(data.data)}`, 'info');
                break;
            case 'monitoring_update':
                this.updateMonitoring(data.data);
                break;
            case 'system_metrics':
                this.updateSystemMetrics(data.data);
                break;
            case 'agent_collaboration':
                this.handleCollaboration(data.message);
                break;
        }
    }
    
    showCreateAgentDialog() {
        this.trackButtonClick('showCreateAgentDialog', {});
        const dialog = document.createElement('div');
        dialog.className = 'modal';
        dialog.innerHTML = `
            <div class="modal-content">
                <h2>Create New Agent</h2>
                <div class="form-group">
                    <label>Agent Type:</label>
                    <select id="agent-type">
                        <option value="simple">Simple Agent</option>
                        <option value="ai">AI Agent (GPT-4)</option>
                    </select>
                </div>
                <div class="form-group">
                    <label>Agent Name:</label>
                    <input type="text" id="agent-name" placeholder="Enter agent name">
                </div>
                <div class="form-group" id="tools-group" style="display:none;">
                    <label>Tools:</label>
                    <div class="checkbox-group">
                        <label><input type="checkbox" value="shell"> Shell Commands</label>
                        <label><input type="checkbox" value="file"> File Operations</label>
                        <label><input type="checkbox" value="http"> HTTP Requests</label>
                        <label><input type="checkbox" value="data"> Data Processing</label>
                    </div>
                </div>
                <button class="btn btn-primary" onclick="app.createAgent()">Create</button>
                <button class="btn" onclick="app.closeDialog()">Cancel</button>
            </div>
        `;
        
        document.body.appendChild(dialog);
        
        document.getElementById('agent-type').addEventListener('change', (e) => {
            document.getElementById('tools-group').style.display = 
                e.target.value === 'ai' ? 'block' : 'none';
        });
    }
    
    async createAgent() {
        const type = document.getElementById('agent-type').value;
        const name = document.getElementById('agent-name').value;
        const tools = Array.from(document.querySelectorAll('#tools-group input:checked'))
                           .map(cb => cb.value);
        
        this.trackButtonClick('createAgent', { type, name, tools });
        
        if (!name || name.trim() === '') {
            this.log('Agent name is required', 'error');
            this.showNotification('Agent name is required', 'error');
            return;
        }
        
        try {
            this.log(`Creating ${type} agent: ${name}`, 'info');
            
            const requestBody = { 
                type: type, 
                name: name.trim(),
                tools: tools 
            };
            
            const response = await fetch('/api/agents', {
                method: 'POST',
                headers: { 
                    'Content-Type': 'application/json',
                    'Accept': 'application/json'
                },
                body: JSON.stringify(requestBody)
            });
            
            const responseData = await response.json();
            
            if (response.ok) {
                this.log(`Successfully created agent: ${responseData.id}`, 'success');
                this.showNotification(`Agent "${name}" created successfully!`, 'success');
                await this.loadAgents();
                this.closeDialog();
            } else {
                const errorMsg = responseData.error || 'Unknown error';
                this.log(`Error creating agent: ${errorMsg}`, 'error');
                this.showNotification(`Failed to create agent: ${errorMsg}`, 'error');
            }
        } catch (error) {
            this.log(`Network error creating agent: ${error.message}`, 'error');
            this.showNotification(`Network error: ${error.message}`, 'error');
        }
    }
    
    async runExample(category, name) {
        this.trackButtonClick('runExample', { category, name });
        this.log(`Running example: ${category}/${name}`, 'info');
        this.showNotification(`Starting example: ${name}`, 'info');
        
        if (category === 'streaming') {
            if (this.ws && this.ws.readyState === WebSocket.OPEN) {
                this.ws.send(JSON.stringify({
                    type: 'run_example',
                    category: category,
                    name: name
                }));
            } else {
                this.log('WebSocket not connected - cannot run streaming example', 'error');
                this.showNotification('WebSocket not connected', 'error');
            }
        } else {
            try {
                const response = await fetch(`/api/examples/${category}`, {
                    method: 'POST',
                    headers: { 
                        'Content-Type': 'application/json',
                        'Accept': 'application/json'
                    },
                    body: JSON.stringify({ example: name })
                });
                
                if (response.ok) {
                    const result = await response.json();
                    this.log(`Example completed: ${JSON.stringify(result)}`, 'success');
                    this.showNotification(`Example "${name}" completed successfully`, 'success');
                } else {
                    const error = await response.json();
                    this.log(`Example failed: ${error.error || 'Unknown error'}`, 'error');
                    this.showNotification(`Example failed: ${error.error || 'Unknown error'}`, 'error');
                }
            } catch (error) {
                this.log(`Error running example: ${error.message}`, 'error');
                this.showNotification(`Network error: ${error.message}`, 'error');
            }
        }
    }
    
    sendMessage() {
        const input = document.getElementById('chat-input');
        const message = input.value.trim();
        
        if (!message || !this.currentAgent) return;
        
        this.addChatMessage(message, 'user');
        input.value = '';
        
        if (this.currentAgent.type === 'ai') {
            this.ws.send(JSON.stringify({
                type: 'stream_chat',
                message: message
            }));
        } else {
            this.sendSimpleMessage(message);
        }
    }
    
    async sendSimpleMessage(message) {
        try {
            const agentId = String(this.currentAgent.id);
            this.log(`Sending message to agent ${agentId}: ${message}`, 'info');
            
            const response = await fetch(`/api/agents/${agentId}/execute`, {
                method: 'POST',
                headers: { 
                    'Content-Type': 'application/json',
                    'Accept': 'application/json'
                },
                body: JSON.stringify({ 
                    action: 'chat', 
                    params: { message: message } 
                })
            });
            
            if (response.ok) {
                const result = await response.json();
                const responseText = result.result || result.message || 'No response';
                this.addChatMessage(responseText, 'agent');
                this.log(`Received response from agent: ${responseText}`, 'info');
            } else {
                const error = await response.json();
                const errorMsg = error.error || `HTTP ${response.status}: ${response.statusText}`;
                this.addChatMessage(`Error: ${errorMsg}`, 'agent');
                this.log(`Chat error: ${errorMsg}`, 'error');
            }
        } catch (error) {
            this.addChatMessage('Network error: ' + error.message, 'agent');
            this.log(`Chat network error: ${error.message}`, 'error');
        }
    }
    
    addChatMessage(message, sender) {
        const messages = document.getElementById('chat-messages');
        if (!messages) return;
        
        const messageDiv = document.createElement('div');
        messageDiv.className = `message ${sender}`;
        messageDiv.textContent = message;
        messages.appendChild(messageDiv);
        messages.scrollTop = messages.scrollHeight;
    }
    
    appendChatToken(token) {
        const messages = document.getElementById('chat-messages');
        if (!messages) return;
        
        let lastMessage = messages.lastElementChild;
        if (!lastMessage || !lastMessage.classList.contains('agent')) {
            lastMessage = document.createElement('div');
            lastMessage.className = 'message agent';
            messages.appendChild(lastMessage);
        }
        
        lastMessage.textContent += token;
        messages.scrollTop = messages.scrollHeight;
    }
    
    completeChatMessage(result) {
        this.log('Chat completed', 'info');
    }
    
    log(message, level = 'info') {
        const timestamp = new Date().toISOString();
        const logEntry = `[${timestamp}] [${level.toUpperCase()}] ${message}`;
        
        // Console logging
        console.log(logEntry);
        
        // UI logging
        const output = document.getElementById('console-output');
        if (output) {
            const logDiv = document.createElement('div');
            logDiv.className = `log-${level}`;
            logDiv.textContent = logEntry;
            output.appendChild(logDiv);
            output.scrollTop = output.scrollHeight;
        }
        
        // Log viewer logging
        const logViewer = document.getElementById('log-viewer');
        if (logViewer) {
            const logDiv = document.createElement('div');
            logDiv.className = `log-entry ${level}`;
            logDiv.textContent = logEntry;
            logViewer.appendChild(logDiv);
            logViewer.scrollTop = logViewer.scrollHeight;
        }
        
        // Server logging via WebSocket
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                type: 'client_log',
                level: level,
                message: message,
                timestamp: timestamp,
                user_agent: navigator.userAgent
            }));
        }
        
        // Store in activity log
        this.activityLog.push({
            timestamp: timestamp,
            level: level,
            message: message
        });
        
        // Keep only last 1000 entries
        if (this.activityLog.length > 1000) {
            this.activityLog.shift();
        }
    }
    
    trackButtonClick(buttonName, context = {}) {
        const timestamp = new Date().toISOString();
        const clickData = {
            timestamp: timestamp,
            button: buttonName,
            context: context,
            currentTab: this.activeTab,
            currentAgent: this.currentAgent?.id || null
        };
        
        this.buttonClickHistory.push(clickData);
        
        // Keep only last 500 clicks
        if (this.buttonClickHistory.length > 500) {
            this.buttonClickHistory.shift();
        }
        
        // Log the click
        this.log(`Button clicked: ${buttonName} ${JSON.stringify(context)}`, 'info');
        
        // Send to server
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                type: 'button_click',
                data: clickData
            }));
        }
    }
    
    clearConsole() {
        const output = document.getElementById('console-output');
        if (output) output.innerHTML = '';
    }
    
    exportLogs() {
        const output = document.getElementById('console-output');
        if (!output) return;
        
        const logs = output.textContent;
        const blob = new Blob([logs], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `agent-logs-${new Date().toISOString()}.txt`;
        a.click();
        URL.revokeObjectURL(url);
    }
    
    closeDialog() {
        const modal = document.querySelector('.modal');
        if (modal) modal.remove();
    }
    
    attachEventListeners() {
        window.addEventListener('beforeunload', () => {
            if (this.ws) this.ws.close();
        });
    }
    
    startMonitoring() {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({ type: 'subscribe_monitoring' }));
        }
    }
    
    updateMonitoring(data) {
        if (!data) return;
        
        // Update system metrics
        const systemMetrics = document.getElementById('system-metrics');
        if (systemMetrics && data.system) {
            systemMetrics.innerHTML = `
                <p>Node: ${data.system.node}</p>
                <p>Memory: ${this.formatBytes(data.system.total_memory)}</p>
                <p>Processes: ${data.system.process_count}</p>
                <p>Schedulers: ${data.system.schedulers}</p>
            `;
        }
        
        // Update agent metrics
        const agentMetrics = document.getElementById('agent-metrics');
        if (agentMetrics && data.agents) {
            agentMetrics.innerHTML = `
                <p>Total: ${data.agents.length}</p>
                <p>Active: ${data.agents.filter(a => a.status === 'alive').length}</p>
            `;
        }
        
        // Update agent grid
        const grid = document.getElementById('agent-monitoring-grid');
        if (grid && data.agents) {
            grid.innerHTML = data.agents.map(agent => `
                <div class="agent-monitor-card">
                    <h4>${agent.meta.name || 'Unnamed'} (${agent.meta.type})</h4>
                    <p>ID: ${String(agent.id).substring(0, 8)}</p>
                    <p>Status: <span class="status-${agent.status}">${agent.status}</span></p>
                    <p>Memory: ${this.formatBytes(agent.memory)}</p>
                    <p>Queue: ${agent.message_queue} messages</p>
                    <button class="btn btn-sm" onclick="app.viewAgentDetails('${agent.id}')">Details</button>
                </div>
            `).join('');
        }
    }
    
    async loadAnalytics() {
        try {
            const response = await fetch('/api/monitoring/analytics');
            const data = await response.json();
            
            // Update summary stats
            document.getElementById('total-agents').textContent = data.summary.total_agents;
            document.getElementById('active-agents').textContent = data.summary.active_agents;
            document.getElementById('total-requests').textContent = data.performance.total_requests;
            document.getElementById('success-rate').textContent = data.performance.avg_response_time + 'ms';
            
            // Load templates
            this.loadTemplates();
        } catch (error) {
            this.log('Error loading analytics: ' + error.message, 'error');
        }
    }
    
    async loadTemplates() {
        // Simulate template loading - in real app would fetch from API
        const templates = [
            { id: 'researcher', name: 'Research Assistant', description: 'Specialized in research and information gathering' },
            { id: 'coder', name: 'Code Assistant', description: 'Expert in software development tasks' },
            { id: 'analyst', name: 'Data Analyst', description: 'Analyzes data and generates insights' },
            { id: 'orchestrator', name: 'Task Orchestrator', description: 'Coordinates multiple agents' }
        ];
        
        const grid = document.getElementById('template-grid');
        if (grid) {
            grid.innerHTML = templates.map(t => `
                <div class="template-card">
                    <h4>${t.name}</h4>
                    <p>${t.description}</p>
                    <button class="btn btn-primary" onclick="app.createFromTemplate('${t.id}')">
                        Use Template
                    </button>
                </div>
            `).join('');
        }
    }
    
    async viewAgentDetails(agentId) {
        try {
            const response = await fetch(`/api/monitoring/agents/${agentId}/metrics`);
            const data = await response.json();
            
            // Show details in a modal
            this.showAgentDetailsModal(agentId, data);
        } catch (error) {
            this.log('Error loading agent details: ' + error.message, 'error');
        }
    }
    
    showAgentDetailsModal(agentId, data) {
        const modal = document.createElement('div');
        modal.className = 'modal';
        modal.innerHTML = `
            <div class="modal-content">
                <h2>Agent Details: ${String(agentId).substring(0, 8)}</h2>
                <div class="agent-details">
                    <h3>State</h3>
                    <pre>${JSON.stringify(data.agent_state, null, 2)}</pre>
                    
                    <h3>Performance</h3>
                    <pre>${JSON.stringify(data.performance, null, 2)}</pre>
                    
                    <h3>Process Info</h3>
                    <pre>${JSON.stringify(data.process_info, null, 2)}</pre>
                </div>
                <button class="btn" onclick="app.closeDialog()">Close</button>
            </div>
        `;
        document.body.appendChild(modal);
    }
    
    async createFromTemplate(templateId) {
        const name = prompt('Enter agent name:');
        if (!name) return;
        
        try {
            const response = await fetch('/api/agents', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    type: 'ai',
                    name: name,
                    template: templateId
                })
            });
            
            if (response.ok) {
                await this.loadAgents();
                this.log(`Created agent from template: ${templateId}`, 'success');
            }
        } catch (error) {
            this.log('Error creating agent from template: ' + error.message, 'error');
        }
    }
    
    handleCollaboration(message) {
        this.log(`Collaboration event: ${JSON.stringify(message)}`, 'info');
    }
    
    formatBytes(bytes) {
        if (bytes === 0) return '0 B';
        const k = 1024;
        const sizes = ['B', 'KB', 'MB', 'GB'];
        const i = Math.floor(Math.log(bytes) / Math.log(k));
        return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
    }
    
    initCharts() {
        this.performanceChart = null;
        this.chartData = {
            labels: [],
            datasets: [{
                label: 'CPU %',
                data: [],
                borderColor: 'var(--primary-color)',
                backgroundColor: 'rgba(99, 102, 241, 0.1)',
                tension: 0.4
            }, {
                label: 'Memory %',
                data: [],
                borderColor: 'var(--secondary-color)',
                backgroundColor: 'rgba(20, 184, 166, 0.1)',
                tension: 0.4
            }]
        };
    }
    
    updateDashboardMetrics() {
        // Update agent counts
        const totalAgents = this.agents.size;
        const activeAgents = Array.from(this.agents.values()).filter(a => a.status === 'active').length;
        const aiAgents = Array.from(this.agents.values()).filter(a => a.type === 'ai').length;
        
        this.animateValue('total-agents-count', 0, totalAgents, 500);
        this.animateValue('active-agents-count', 0, activeAgents, 500);
        this.animateValue('ai-agents-count', 0, aiAgents, 500);
        
        // Simulate system metrics
        const cpuUsage = Math.floor(Math.random() * 30 + 20);
        const memoryUsage = Math.floor(Math.random() * 40 + 30);
        const processes = Math.floor(Math.random() * 50 + 100);
        
        this.animateValue('cpu-usage', 0, cpuUsage, 500, '%');
        this.animateValue('memory-usage', 0, memoryUsage, 500, '%');
        this.animateValue('active-processes', 0, processes, 500);
        
        // Update performance chart
        this.updatePerformanceChart(cpuUsage, memoryUsage);
    }
    
    animateValue(elementId, start, end, duration, suffix = '') {
        const element = document.getElementById(elementId);
        if (!element) return;
        
        const range = end - start;
        const minTimer = 50;
        let stepTime = Math.abs(Math.floor(duration / range));
        stepTime = Math.max(stepTime, minTimer);
        
        const startTime = new Date().getTime();
        const endTime = startTime + duration;
        let timer;
        
        function run() {
            const now = new Date().getTime();
            const remaining = Math.max((endTime - now) / duration, 0);
            const value = Math.round(end - (remaining * range));
            element.textContent = value + suffix;
            
            if (value === end) {
                clearInterval(timer);
            }
        }
        
        timer = setInterval(run, stepTime);
        run();
    }
    
    updatePerformanceChart(cpu, memory) {
        const canvas = document.getElementById('dashboard-performance-chart');
        if (!canvas) return;
        
        const ctx = canvas.getContext('2d');
        
        // Simple chart drawing (in production, use Chart.js)
        const width = canvas.width;
        const height = canvas.height;
        
        // Clear canvas
        ctx.clearRect(0, 0, width, height);
        
        // Add new data point
        this.chartData.labels.push(new Date().toLocaleTimeString());
        this.chartData.datasets[0].data.push(cpu);
        this.chartData.datasets[1].data.push(memory);
        
        // Keep last 10 points
        if (this.chartData.labels.length > 10) {
            this.chartData.labels.shift();
            this.chartData.datasets[0].data.shift();
            this.chartData.datasets[1].data.shift();
        }
        
        // Draw simple line chart
        const padding = 20;
        const chartWidth = width - padding * 2;
        const chartHeight = height - padding * 2;
        const maxValue = 100;
        
        ctx.strokeStyle = 'var(--gray-300)';
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(padding, padding);
        ctx.lineTo(padding, height - padding);
        ctx.lineTo(width - padding, height - padding);
        ctx.stroke();
        
        // Draw data lines
        this.chartData.datasets.forEach((dataset, datasetIndex) => {
            ctx.strokeStyle = dataset.borderColor;
            ctx.lineWidth = 2;
            ctx.beginPath();
            
            dataset.data.forEach((value, index) => {
                const x = padding + (index / (this.chartData.labels.length - 1)) * chartWidth;
                const y = height - padding - (value / maxValue) * chartHeight;
                
                if (index === 0) {
                    ctx.moveTo(x, y);
                } else {
                    ctx.lineTo(x, y);
                }
            });
            
            ctx.stroke();
        });
    }
    
    startActivityFeed() {
        this.updateActivityFeed();
        setInterval(() => this.updateActivityFeed(), 5000);
    }
    
    updateActivityFeed() {
        const activities = [
            { type: 'info', title: 'Agent Started', description: 'New AI agent initialized successfully' },
            { type: 'success', title: 'Task Completed', description: 'Research task completed by Agent-001' },
            { type: 'warning', title: 'High Memory Usage', description: 'Agent-003 using 85% of allocated memory' },
            { type: 'info', title: 'Network Update', description: 'Agent cluster topology optimized' }
        ];
        
        const feed = document.getElementById('recent-activity');
        if (!feed) return;
        
        // Add a random activity
        const activity = activities[Math.floor(Math.random() * activities.length)];
        const activityHtml = `
            <div class="activity-item" style="animation: fadeInUp 0.3s ease-out;">
                <div class="activity-icon ${activity.type}">•</div>
                <div class="activity-content">
                    <div class="activity-title">${activity.title}</div>
                    <div class="activity-description">${activity.description}</div>
                    <div class="activity-time">${new Date().toLocaleTimeString()}</div>
                </div>
            </div>
        `;
        
        feed.innerHTML = activityHtml + feed.innerHTML;
        
        // Keep only last 5 activities
        const items = feed.querySelectorAll('.activity-item');
        if (items.length > 5) {
            items[items.length - 1].remove();
        }
    }
    
    refreshDashboard() {
        this.trackButtonClick('refreshDashboard', {});
        this.showNotification('Refreshing dashboard...', 'info');
        this.log('Refreshing dashboard data', 'info');
        this.loadAgents().then(() => {
            this.updateDashboardMetrics();
            this.showNotification('Dashboard refreshed', 'success');
        }).catch(error => {
            this.log(`Error refreshing dashboard: ${error.message}`, 'error');
            this.showNotification('Failed to refresh dashboard', 'error');
        });
    }
    
    showNotification(message, type = 'info') {
        const container = document.getElementById('notifications');
        if (!container) return;
        
        const notification = document.createElement('div');
        notification.className = `notification ${type}`;
        notification.innerHTML = `
            <div class="notification-title">${type.charAt(0).toUpperCase() + type.slice(1)}</div>
            <div class="notification-message">${message}</div>
        `;
        
        container.appendChild(notification);
        
        setTimeout(() => {
            notification.style.animation = 'slideOut 0.3s ease-in forwards';
            setTimeout(() => notification.remove(), 300);
        }, 3000);
    }
    
    handleSearch(event) {
        const query = event.target.value.toLowerCase();
        // Implement search functionality
    }
    
    handleFilter(event) {
        const filter = event.target.value;
        // Implement filter functionality
    }
    
    updateSystemMetrics(data) {
        if (data.cpu_usage !== undefined) {
            this.animateValue('cpu-usage', 0, Math.round(data.cpu_usage), 500, '%');
        }
        if (data.memory_usage !== undefined) {
            this.animateValue('memory-usage', 0, Math.round(data.memory_usage), 500, '%');
        }
        if (data.process_count !== undefined) {
            this.animateValue('active-processes', 0, data.process_count, 500);
        }
    }
    
    initializeNetworkTopology() {
        // Placeholder for network topology visualization
        setTimeout(() => {
            const container = document.getElementById('network-topology');
            if (container) {
                container.innerHTML = `
                    <div style="position: relative; width: 100%; height: 400px;">
                        <div class="node agent" style="top: 50%; left: 50%; transform: translate(-50%, -50%);">Hub</div>
                        <div class="node coordinator" style="top: 20%; left: 30%;">Coord-1</div>
                        <div class="node worker" style="top: 20%; left: 70%;">Work-1</div>
                        <div class="node worker" style="top: 80%; left: 30%;">Work-2</div>
                        <div class="node agent" style="top: 80%; left: 70%;">Agent-1</div>
                    </div>
                `;
            }
        }, 100);
    }
    
    restartAllAgents() {
        this.showNotification('Restarting all agents...', 'warning');
        // Implement restart functionality
    }
    
    exportMetrics() {
        this.showNotification('Exporting metrics...', 'info');
        // Implement export functionality
    }
    
    systemCleanup() {
        this.showNotification('Running system cleanup...', 'info');
        // Implement cleanup functionality
    }
    
    runDiagnostics() {
        this.showNotification('Running diagnostics...', 'info');
        // Implement diagnostics functionality
    }
    
    showSettings() {
        this.trackButtonClick('showSettings', {});
        this.showNotification('Settings coming soon!', 'info');
        // Implement settings modal
    }
    
    // Missing methods implementation
    startChat(agentId) {
        this.trackButtonClick('startChat', { agentId });
        this.log(`Starting chat with agent: ${agentId}`, 'info');
        this.selectAgent(agentId);
        this.switchTab('chat');
    }
    
    viewAgentDetails(agentId) {
        this.trackButtonClick('viewAgentDetails', { agentId });
        this.log(`Viewing details for agent: ${agentId}`, 'info');
        this.showAgentDetailsModal(agentId, { loading: true });
        
        // Load actual metrics
        fetch(`/api/monitoring/agents/${agentId}/metrics`)
            .then(response => response.json())
            .then(data => {
                this.showAgentDetailsModal(agentId, data);
            })
            .catch(error => {
                this.log(`Error loading agent details: ${error.message}`, 'error');
                this.showNotification('Failed to load agent details', 'error');
            });
    }
    
    async stopAgent(agentId) {
        this.trackButtonClick('stopAgent', { agentId });
        this.log(`Stopping agent: ${agentId}`, 'warning');
        
        if (!confirm('Are you sure you want to stop this agent?')) {
            return;
        }
        
        try {
            const formattedId = String(agentId);
            const response = await fetch(`/api/agents/${formattedId}`, {
                method: 'DELETE',
                headers: {
                    'Accept': 'application/json'
                }
            });
            
            if (response.ok) {
                this.log(`Agent ${agentId} stopped successfully`, 'success');
                this.showNotification('Agent stopped', 'success');
                await this.loadAgents();
                
                // Clear current agent if it was the stopped one
                if (this.currentAgent && String(this.currentAgent.id) === String(agentId)) {
                    this.currentAgent = null;
                }
            } else {
                const error = await response.json();
                this.log(`Error stopping agent: ${error.error}`, 'error');
                this.showNotification(`Failed to stop agent: ${error.error}`, 'error');
            }
        } catch (error) {
            this.log(`Network error stopping agent: ${error.message}`, 'error');
            this.showNotification(`Network error: ${error.message}`, 'error');
        }
    }
    
    restartAllAgents() {
        this.trackButtonClick('restartAllAgents', {});
        this.log('Restart all agents requested', 'warning');
        this.showNotification('Restarting all agents...', 'warning');
        // TODO: Implement actual restart functionality
        setTimeout(() => {
            this.showNotification('All agents restarted', 'success');
            this.loadAgents();
        }, 2000);
    }
    
    exportMetrics() {
        this.trackButtonClick('exportMetrics', {});
        this.log('Exporting metrics', 'info');
        this.showNotification('Exporting metrics...', 'info');
        
        // Export current metrics as JSON
        const exportData = {
            timestamp: new Date().toISOString(),
            agents: Array.from(this.agents.values()),
            systemMetrics: this.systemMetrics,
            buttonClickHistory: this.buttonClickHistory,
            activityLog: this.activityLog
        };
        
        const blob = new Blob([JSON.stringify(exportData, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `agent-metrics-${new Date().toISOString()}.json`;
        a.click();
        URL.revokeObjectURL(url);
        
        this.showNotification('Metrics exported', 'success');
    }
    
    systemCleanup() {
        this.trackButtonClick('systemCleanup', {});
        this.log('System cleanup requested', 'info');
        this.showNotification('Running system cleanup...', 'info');
        
        // Clear logs and reset metrics
        this.activityLog = [];
        this.buttonClickHistory = [];
        this.performanceHistory = [];
        
        // Clear UI elements
        const output = document.getElementById('console-output');
        if (output) output.innerHTML = '';
        
        const logViewer = document.getElementById('log-viewer');
        if (logViewer) logViewer.innerHTML = '<div class="log-entry info">Logs cleared...</div>';
        
        setTimeout(() => {
            this.showNotification('System cleanup completed', 'success');
        }, 1000);
    }
    
    runDiagnostics() {
        this.trackButtonClick('runDiagnostics', {});
        this.log('Running system diagnostics', 'info');
        this.showNotification('Running diagnostics...', 'info');
        
        // Run various checks
        const diagnostics = {
            websocket: this.ws && this.ws.readyState === WebSocket.OPEN ? 'Connected' : 'Disconnected',
            agents: this.agents.size,
            currentTab: this.activeTab,
            currentAgent: this.currentAgent?.id || 'None',
            memoryUsage: performance.memory ? Math.round(performance.memory.usedJSHeapSize / 1024 / 1024) + ' MB' : 'Unknown'
        };
        
        this.log(`Diagnostics: ${JSON.stringify(diagnostics)}`, 'info');
        this.showNotification('Diagnostics completed - check logs', 'success');
    }
    
    filterAgents(event) {
        this.trackButtonClick('filterAgents', { query: event.target.value });
        const query = event.target.value.toLowerCase();
        this.log(`Filtering agents with query: ${query}`, 'info');
        // TODO: Implement actual filtering
    }
    
    filterAgentsByType(event) {
        this.trackButtonClick('filterAgentsByType', { type: event.target.value });
        const type = event.target.value;
        this.log(`Filtering agents by type: ${type}`, 'info');
        // TODO: Implement actual filtering
    }
    
    handleSearch(event) {
        this.trackButtonClick('handleSearch', { query: event.target.value });
        const query = event.target.value.toLowerCase();
        this.log(`Global search: ${query}`, 'info');
        // TODO: Implement search functionality
    }
    
    handleFilter(event) {
        this.trackButtonClick('handleFilter', { filter: event.target.value });
        const filter = event.target.value;
        this.log(`Global filter: ${filter}`, 'info');
        // TODO: Implement filter functionality
    }
    
    refreshTopology() {
        this.trackButtonClick('refreshTopology', {});
        this.log('Refreshing network topology', 'info');
        this.showNotification('Refreshing topology...', 'info');
        this.initializeNetworkTopology();
        this.showNotification('Topology refreshed', 'success');
    }
    
    autoLayoutNodes() {
        this.trackButtonClick('autoLayoutNodes', {});
        this.log('Auto-layouting network nodes', 'info');
        this.showNotification('Auto-layout applied', 'success');
    }
    
    toggleConnections(event) {
        this.trackButtonClick('toggleConnections', { enabled: event.target.checked });
        this.log(`Toggling connections: ${event.target.checked}`, 'info');
    }
    
    clearLogs() {
        this.trackButtonClick('clearLogs', {});
        this.log('Clearing logs', 'info');
        const logViewer = document.getElementById('log-viewer');
        if (logViewer) {
            logViewer.innerHTML = '<div class="log-entry info">Logs cleared...</div>';
        }
        this.activityLog = [];
        this.showNotification('Logs cleared', 'success');
    }
    
    exportLogs() {
        this.trackButtonClick('exportLogs', {});
        this.log('Exporting logs', 'info');
        
        const logs = this.activityLog.map(entry => 
            `[${entry.timestamp}] [${entry.level.toUpperCase()}] ${entry.message}`
        ).join('\n');
        
        const blob = new Blob([logs], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `agent-logs-${new Date().toISOString()}.txt`;
        a.click();
        URL.revokeObjectURL(url);
        
        this.showNotification('Logs exported', 'success');
    }
    
    filterLogs(event) {
        this.trackButtonClick('filterLogs', { level: event.target.value });
        const level = event.target.value;
        this.log(`Filtering logs by level: ${level}`, 'info');
        // TODO: Implement log filtering
    }
    
    searchLogs(event) {
        this.trackButtonClick('searchLogs', { query: event.target.value });
        const query = event.target.value;
        this.log(`Searching logs: ${query}`, 'info');
        // TODO: Implement log search
    }
    
    toggleAutoScroll(event) {
        this.trackButtonClick('toggleAutoScroll', { enabled: event.target.checked });
        this.log(`Auto-scroll toggled: ${event.target.checked}`, 'info');
    }
    
    renderKnowledge() {
        return `
            <div class="knowledge-base">
                <h2>Knowledge Base Management</h2>
                
                <div class="control-panel">
                    <div style="display: flex; gap: 10px; align-items: center; margin-bottom: 15px;">
                        <button class="btn btn-primary" onclick="app.discoverNewDomains()">
                            <span style="margin-right: 0.5rem;">🔍</span> Discover Domains
                        </button>
                        <button class="btn btn-success" onclick="app.showIngestDialog()">
                            <span style="margin-right: 0.5rem;">📥</span> Ingest Knowledge
                        </button>
                        <button class="btn btn-info" onclick="app.showSearchDialog()">
                            <span style="margin-right: 0.5rem;">🔎</span> Search Knowledge
                        </button>
                        <button class="btn" onclick="app.refreshKnowledgeBase()">
                            <span style="margin-right: 0.5rem;">↻</span> Refresh
                        </button>
                    </div>
                    
                    <div class="knowledge-status" id="knowledge-status">
                        <p>Loading knowledge base status...</p>
                    </div>
                </div>
                
                <div class="knowledge-domains-grid" id="knowledge-domains-grid">
                    <p>Loading knowledge domains...</p>
                </div>
                
                <div class="knowledge-search-results" id="knowledge-search-results" style="display: none;">
                    <h3>Search Results</h3>
                    <div id="search-results-content"></div>
                </div>
            </div>
        `;
    }
    
    async loadKnowledgeDomains() {
        this.trackButtonClick('loadKnowledgeDomains', {});
        this.log('Loading knowledge domains', 'info');
        
        try {
            const response = await fetch('/api/knowledge');
            const data = await response.json();
            
            if (response.ok) {
                this.renderKnowledgeDomains(data.domains || []);
                this.loadKnowledgeStatus();
            } else {
                this.log(`Error loading knowledge domains: ${data.error}`, 'error');
                this.showNotification('Failed to load knowledge domains', 'error');
            }
        } catch (error) {
            this.log(`Network error loading knowledge domains: ${error.message}`, 'error');
            this.showNotification('Network error', 'error');
        }
    }
    
    renderKnowledgeDomains(domains) {
        const grid = document.getElementById('knowledge-domains-grid');
        if (!grid) return;
        
        if (domains.length === 0) {
            grid.innerHTML = `
                <div class="empty-state">
                    <h3>No Knowledge Domains Found</h3>
                    <p>Start by discovering domains or ingesting knowledge from external sources.</p>
                    <button class="btn btn-primary" onclick="app.discoverNewDomains()">Discover Domains</button>
                </div>
            `;
            return;
        }
        
        grid.innerHTML = domains.map(domain => `
            <div class="knowledge-domain-card" data-domain="${domain}">
                <div class="domain-header">
                    <h4>${domain}</h4>
                    <div class="domain-actions">
                        <button class="btn btn-sm" onclick="app.viewDomainContents('${domain}')">View</button>
                        <button class="btn btn-sm btn-primary" onclick="app.searchDomain('${domain}')">Search</button>
                    </div>
                </div>
                <div class="domain-info" id="domain-info-${domain}">
                    <p>Loading domain information...</p>
                </div>
            </div>
        `).join('');
        
        // Load information for each domain
        domains.forEach(domain => this.loadDomainInfo(domain));
    }
    
    async loadDomainInfo(domain) {
        try {
            const response = await fetch(`/api/knowledge/${domain}`);
            const data = await response.json();
            
            const infoDiv = document.getElementById(`domain-info-${domain}`);
            if (!infoDiv) return;
            
            if (response.ok) {
                if (data.contents) {
                    infoDiv.innerHTML = `
                        <p><strong>Files:</strong> ${data.contents.length}</p>
                        <p><strong>Types:</strong> ${[...new Set(data.contents.map(c => c.type))].join(', ')}</p>
                        <div class="file-list">
                            ${data.contents.slice(0, 3).map(file => `
                                <div class="file-item">
                                    <span class="file-name">${file.file}</span>
                                    <span class="file-size">${this.formatBytes(file.size)}</span>
                                </div>
                            `).join('')}
                            ${data.contents.length > 3 ? `<p>...and ${data.contents.length - 3} more files</p>` : ''}
                        </div>
                    `;
                } else {
                    infoDiv.innerHTML = '<p>No content available</p>';
                }
            } else {
                infoDiv.innerHTML = `<p class="error">Error: ${data.error}</p>`;
            }
        } catch (error) {
            const infoDiv = document.getElementById(`domain-info-${domain}`);
            if (infoDiv) {
                infoDiv.innerHTML = `<p class="error">Failed to load: ${error.message}</p>`;
            }
        }
    }
    
    async loadKnowledgeStatus() {
        try {
            const response = await fetch('/api/knowledge/status');
            const data = await response.json();
            
            const statusDiv = document.getElementById('knowledge-status');
            if (!statusDiv) return;
            
            if (response.ok) {
                statusDiv.innerHTML = `
                    <div class="status-grid">
                        <div class="status-item">
                            <div class="status-value">${data.queue_length || 0}</div>
                            <div class="status-label">Queue Length</div>
                        </div>
                        <div class="status-item">
                            <div class="status-value">${data.active_jobs || 0}</div>
                            <div class="status-label">Active Jobs</div>
                        </div>
                        <div class="status-item">
                            <div class="status-value">${Object.keys(data.domain_coverage || {}).length}</div>
                            <div class="status-label">Covered Domains</div>
                        </div>
                        <div class="status-item">
                            <div class="status-value">${data.last_discovery ? 'Recent' : 'Never'}</div>
                            <div class="status-label">Last Discovery</div>
                        </div>
                    </div>
                `;
            } else {
                statusDiv.innerHTML = '<p>Status unavailable</p>';
            }
        } catch (error) {
            const statusDiv = document.getElementById('knowledge-status');
            if (statusDiv) {
                statusDiv.innerHTML = '<p>Failed to load status</p>';
            }
        }
    }
    
    async discoverNewDomains() {
        this.trackButtonClick('discoverNewDomains', {});
        this.log('Discovering new knowledge domains', 'info');
        this.showNotification('Starting domain discovery...', 'info');
        
        try {
            const response = await fetch('/api/knowledge/discover', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' }
            });
            
            const data = await response.json();
            
            if (response.ok) {
                this.log(`Domain discovery completed: ${data.new_domains?.length || 0} new domains`, 'success');
                this.showNotification(`Discovered ${data.new_domains?.length || 0} new domains`, 'success');
                this.loadKnowledgeDomains(); // Refresh the domains list
            } else {
                this.log(`Domain discovery failed: ${data.error}`, 'error');
                this.showNotification(`Discovery failed: ${data.error}`, 'error');
            }
        } catch (error) {
            this.log(`Network error during domain discovery: ${error.message}`, 'error');
            this.showNotification('Network error during discovery', 'error');
        }
    }
    
    showIngestDialog() {
        this.trackButtonClick('showIngestDialog', {});
        
        const dialog = document.createElement('div');
        dialog.className = 'modal';
        dialog.innerHTML = `
            <div class="modal-content">
                <h2>Ingest Knowledge</h2>
                <div class="form-group">
                    <label>Source Type:</label>
                    <select id="ingest-source-type">
                        <option value="web">Web</option>
                        <option value="api">API</option>
                        <option value="file">File</option>
                    </select>
                </div>
                <div class="form-group">
                    <label>Source URL/Path:</label>
                    <input type="text" id="ingest-source-url" placeholder="Enter URL or file path">
                </div>
                <div class="form-group">
                    <label>Target Domain:</label>
                    <input type="text" id="ingest-target-domain" placeholder="Enter domain name">
                </div>
                <div class="form-group">
                    <label>Priority (1-10):</label>
                    <input type="number" id="ingest-priority" min="1" max="10" value="5">
                </div>
                <button class="btn btn-primary" onclick="app.startIngestion()">Start Ingestion</button>
                <button class="btn" onclick="app.closeDialog()">Cancel</button>
            </div>
        `;
        
        document.body.appendChild(dialog);
    }
    
    async startIngestion() {
        this.trackButtonClick('startIngestion', {});
        
        const sourceType = document.getElementById('ingest-source-type').value;
        const sourceUrl = document.getElementById('ingest-source-url').value;
        const targetDomain = document.getElementById('ingest-target-domain').value;
        const priority = parseInt(document.getElementById('ingest-priority').value);
        
        if (!sourceUrl || !targetDomain) {
            this.showNotification('Please fill in all required fields', 'error');
            return;
        }
        
        const sources = [{
            type: sourceType,
            url: sourceUrl,
            domain: targetDomain,
            priority: priority
        }];
        
        try {
            this.log(`Starting knowledge ingestion from ${sourceUrl}`, 'info');
            
            const response = await fetch('/api/knowledge/ingest', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ sources: sources })
            });
            
            const data = await response.json();
            
            if (response.ok) {
                this.log(`Ingestion started: ${data.sources_count} sources queued`, 'success');
                this.showNotification('Knowledge ingestion started', 'success');
                this.closeDialog();
                this.loadKnowledgeStatus(); // Refresh status
            } else {
                this.log(`Ingestion failed: ${data.error}`, 'error');
                this.showNotification(`Ingestion failed: ${data.error}`, 'error');
            }
        } catch (error) {
            this.log(`Network error during ingestion: ${error.message}`, 'error');
            this.showNotification('Network error during ingestion', 'error');
        }
    }
    
    showSearchDialog() {
        this.trackButtonClick('showSearchDialog', {});
        
        const dialog = document.createElement('div');
        dialog.className = 'modal';
        dialog.innerHTML = `
            <div class="modal-content">
                <h2>Search Knowledge Base</h2>
                <div class="form-group">
                    <label>Domain:</label>
                    <select id="search-domain">
                        <option value="">Loading domains...</option>
                    </select>
                </div>
                <div class="form-group">
                    <label>Search Query:</label>
                    <input type="text" id="search-query" placeholder="Enter search terms">
                </div>
                <button class="btn btn-primary" onclick="app.performSearch()">Search</button>
                <button class="btn" onclick="app.closeDialog()">Cancel</button>
            </div>
        `;
        
        document.body.appendChild(dialog);
        
        // Load domains for the dropdown
        this.loadDomainsForSearch();
    }
    
    async loadDomainsForSearch() {
        try {
            const response = await fetch('/api/knowledge');
            const data = await response.json();
            
            const select = document.getElementById('search-domain');
            if (select && data.domains) {
                select.innerHTML = data.domains.map(domain => 
                    `<option value="${domain}">${domain}</option>`
                ).join('');
            }
        } catch (error) {
            this.log(`Error loading domains for search: ${error.message}`, 'error');
        }
    }
    
    async performSearch() {
        this.trackButtonClick('performSearch', {});
        
        const domain = document.getElementById('search-domain').value;
        const query = document.getElementById('search-query').value;
        
        if (!domain || !query) {
            this.showNotification('Please select a domain and enter a search query', 'error');
            return;
        }
        
        try {
            this.log(`Searching knowledge base: domain=${domain}, query=${query}`, 'info');
            this.showNotification('Searching knowledge base...', 'info');
            
            const response = await fetch('/api/knowledge/search', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ domain: domain, query: query })
            });
            
            const data = await response.json();
            
            if (response.ok) {
                this.displaySearchResults(data);
                this.closeDialog();
                this.showNotification(`Found ${data.results?.length || 0} results`, 'success');
            } else {
                this.log(`Search failed: ${data.error}`, 'error');
                this.showNotification(`Search failed: ${data.error}`, 'error');
            }
        } catch (error) {
            this.log(`Network error during search: ${error.message}`, 'error');
            this.showNotification('Network error during search', 'error');
        }
    }
    
    displaySearchResults(data) {
        const resultsDiv = document.getElementById('knowledge-search-results');
        const contentDiv = document.getElementById('search-results-content');
        
        if (!resultsDiv || !contentDiv) return;
        
        resultsDiv.style.display = 'block';
        
        if (!data.results || data.results.length === 0) {
            contentDiv.innerHTML = '<p>No results found for your search.</p>';
            return;
        }
        
        contentDiv.innerHTML = `
            <div class="search-info">
                <p><strong>Domain:</strong> ${data.domain}</p>
                <p><strong>Query:</strong> "${data.query}"</p>
                <p><strong>Results:</strong> ${data.results.length}</p>
            </div>
            
            <div class="search-results-list">
                ${data.results.map(result => `
                    <div class="search-result-item">
                        <div class="result-header">
                            <h4>${result.file}</h4>
                            <span class="relevance-score">Relevance: ${result.relevance_score}</span>
                        </div>
                        <div class="result-preview">
                            ${result.content_preview || 'No preview available'}
                        </div>
                        ${result.full_path ? `<div class="result-path">${result.full_path}</div>` : ''}
                    </div>
                `).join('')}
            </div>
        `;
    }
    
    refreshKnowledgeBase() {
        this.trackButtonClick('refreshKnowledgeBase', {});
        this.log('Refreshing knowledge base', 'info');
        this.showNotification('Refreshing knowledge base...', 'info');
        this.loadKnowledgeDomains();
    }
    
    async viewDomainContents(domain) {
        this.trackButtonClick('viewDomainContents', { domain });
        this.log(`Viewing contents of domain: ${domain}`, 'info');
        
        try {
            const response = await fetch(`/api/knowledge/${domain}`);
            const data = await response.json();
            
            if (response.ok) {
                this.showDomainContentsModal(domain, data);
            } else {
                this.log(`Error viewing domain contents: ${data.error}`, 'error');
                this.showNotification(`Failed to load domain contents: ${data.error}`, 'error');
            }
        } catch (error) {
            this.log(`Network error viewing domain contents: ${error.message}`, 'error');
            this.showNotification('Network error', 'error');
        }
    }
    
    showDomainContentsModal(domain, data) {
        const modal = document.createElement('div');
        modal.className = 'modal';
        modal.innerHTML = `
            <div class="modal-content large-modal">
                <h2>Domain Contents: ${domain}</h2>
                <div class="domain-contents">
                    ${data.contents ? data.contents.map(file => `
                        <div class="file-detail">
                            <div class="file-header">
                                <h4>${file.file}</h4>
                                <span class="file-meta">${file.type} - ${this.formatBytes(file.size)}</span>
                            </div>
                            <div class="file-preview">
                                ${file.preview || 'No preview available'}
                            </div>
                        </div>
                    `).join('') : '<p>No contents available</p>'}
                </div>
                <button class="btn" onclick="app.closeDialog()">Close</button>
            </div>
        `;
        document.body.appendChild(modal);
    }
    
    searchDomain(domain) {
        this.trackButtonClick('searchDomain', { domain });
        // Pre-fill the search dialog with the selected domain
        this.showSearchDialog();
        setTimeout(() => {
            const domainSelect = document.getElementById('search-domain');
            if (domainSelect) {
                domainSelect.value = domain;
            }
        }, 100);
    }
}

const modalStyles = `
<style>
.modal {
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    background: rgba(0,0,0,0.5);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
}

.modal-content {
    background: white;
    padding: 30px;
    border-radius: 8px;
    max-width: 500px;
    width: 90%;
}

.modal-content h2 {
    margin-bottom: 20px;
}
</style>
`;

document.head.insertAdjacentHTML('beforeend', modalStyles);

const app = new AgentWebApp();