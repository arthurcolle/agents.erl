class AgentWebApp {
    constructor() {
        this.agents = new Map();
        this.currentAgent = null;
        this.ws = null;
        this.activeTab = 'overview';
        
        this.initializeApp();
    }
    
    async initializeApp() {
        this.renderUI();
        this.attachEventListeners();
        await this.loadAgents();
        this.connectWebSocket();
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
                        <button class="tab active" data-tab="overview" onclick="app.switchTab('overview')">
                            Overview
                        </button>
                        <button class="tab" data-tab="examples" onclick="app.switchTab('examples')">
                            Examples
                        </button>
                        <button class="tab" data-tab="chat" onclick="app.switchTab('chat')">
                            Chat Interface
                        </button>
                        <button class="tab" data-tab="console" onclick="app.switchTab('console')">
                            Console Output
                        </button>
                        <button class="tab" data-tab="monitoring" onclick="app.switchTab('monitoring')">
                            Monitoring
                        </button>
                        <button class="tab" data-tab="analytics" onclick="app.switchTab('analytics')">
                            Analytics
                        </button>
                    </div>
                    
                    <div class="tab-content" id="tab-content">
                        ${this.renderOverview()}
                    </div>
                </div>
            </div>
        `;
    }
    
    renderOverview() {
        return `
            <div class="overview">
                <h2>System Overview</h2>
                <p>This web interface demonstrates the capabilities of the Erlang Agent System.</p>
                
                <h3>Features:</h3>
                <ul>
                    <li><strong>Distributed Agents:</strong> Deploy agents across multiple nodes</li>
                    <li><strong>AI Integration:</strong> OpenAI GPT models for intelligent agents</li>
                    <li><strong>Tool Composition:</strong> Chain multiple tools for complex workflows</li>
                    <li><strong>Streaming Support:</strong> Real-time data processing and responses</li>
                    <li><strong>Fault Tolerance:</strong> Automatic recovery and supervision</li>
                </ul>
                
                <h3>Getting Started:</h3>
                <ol>
                    <li>Create a new agent using the sidebar button</li>
                    <li>Explore the examples tab to see pre-built demonstrations</li>
                    <li>Use the chat interface to interact with AI agents</li>
                    <li>Monitor system output in the console tab</li>
                </ol>
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
        this.currentAgent = this.agents.get(agentId);
        this.renderAgentList();
        
        if (this.activeTab === 'chat') {
            this.switchTab('chat');
        }
        
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.ws.send(JSON.stringify({
                type: 'create_stream',
                agent_id: agentId
            }));
        }
    }
    
    switchTab(tabName) {
        this.activeTab = tabName;
        
        document.querySelectorAll('.tab').forEach(tab => {
            tab.classList.toggle('active', tab.dataset.tab === tabName);
        });
        
        const content = document.getElementById('tab-content');
        switch (tabName) {
            case 'overview':
                content.innerHTML = this.renderOverview();
                break;
            case 'examples':
                content.innerHTML = this.renderExamples();
                break;
            case 'chat':
                content.innerHTML = this.renderChat();
                break;
            case 'console':
                content.innerHTML = this.renderConsole();
                break;
            case 'monitoring':
                content.innerHTML = this.renderMonitoring();
                this.startMonitoring();
                break;
            case 'analytics':
                content.innerHTML = this.renderAnalytics();
                this.loadAnalytics();
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
        
        try {
            const response = await fetch('/api/agents', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ type, name, tools })
            });
            
            if (response.ok) {
                const agent = await response.json();
                this.log(`Created agent: ${agent.id}`, 'success');
                await this.loadAgents();
                this.closeDialog();
            } else {
                const error = await response.json();
                this.log(`Error creating agent: ${error.error}`, 'error');
            }
        } catch (error) {
            this.log(`Error creating agent: ${error.message}`, 'error');
        }
    }
    
    async runExample(category, name) {
        this.log(`Running example: ${category}/${name}`, 'info');
        
        if (category === 'streaming') {
            this.ws.send(JSON.stringify({
                type: 'run_example',
                category: category,
                name: name
            }));
        } else {
            try {
                const response = await fetch(`/api/examples/${category}`, {
                    method: 'POST',
                    headers: { 'Content-Type': 'application/json' },
                    body: JSON.stringify({ example: name })
                });
                
                const result = await response.json();
                this.log(`Example result: ${JSON.stringify(result)}`, 'info');
            } catch (error) {
                this.log(`Error running example: ${error.message}`, 'error');
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
            const response = await fetch(`/api/agents/${this.currentAgent.id}/execute`, {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({ action: 'chat', params: { message } })
            });
            
            const result = await response.json();
            this.addChatMessage(result.result, 'agent');
        } catch (error) {
            this.addChatMessage('Error: ' + error.message, 'agent');
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
        const output = document.getElementById('console-output');
        if (output) {
            const timestamp = new Date().toLocaleTimeString();
            const logEntry = document.createElement('div');
            logEntry.className = `log-${level}`;
            logEntry.textContent = `[${timestamp}] ${message}`;
            output.appendChild(logEntry);
            output.scrollTop = output.scrollHeight;
        }
        
        console.log(`[${level}] ${message}`);
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