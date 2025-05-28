# Erlang Agent System Web Application

A comprehensive web interface for the Erlang Agent System that demonstrates distributed agents, AI integration, tool composition, and real-time streaming capabilities.

## Features

- **Interactive Web UI**: Modern single-page application for agent management
- **REST API**: Full CRUD operations for agent lifecycle management
- **WebSocket Support**: Real-time streaming for AI responses and updates
- **Example Gallery**: Pre-built demonstrations of all major features
- **Live Chat Interface**: Interact with AI agents in real-time
- **System Console**: Monitor agent activities and system events

## Quick Start

1. **Install Dependencies**:
   ```bash
   ./rebar3 get-deps
   ./rebar3 compile
   ```

2. **Start the Web Application**:
   ```bash
   ./start_web.sh
   ```

3. **Access the Interface**:
   Open your browser to: http://localhost:8080

## Architecture

### Backend Components

- **agent_web_app**: Application entry point
- **agent_web_sup**: Supervisor managing Cowboy web server
- **agent_api_handler**: REST API endpoints for agent management
- **agent_execute_handler**: Agent command execution endpoint
- **examples_handler**: Example demonstration endpoints
- **agent_ws_handler**: WebSocket handler for real-time communication

### Frontend Components

- **Single Page Application**: Vanilla JavaScript with modern ES6+
- **Responsive Design**: Works on desktop and mobile devices
- **Real-time Updates**: WebSocket integration for streaming responses
- **Interactive Examples**: Click-to-run demonstrations

## API Endpoints

### Agent Management

- `GET /api/agents` - List all active agents
- `POST /api/agents` - Create a new agent
  ```json
  {
    "type": "ai",
    "name": "MyAgent",
    "tools": ["shell", "file", "http"]
  }
  ```
- `GET /api/agents/:id` - Get agent details
- `DELETE /api/agents/:id` - Stop and remove an agent
- `POST /api/agents/:id/execute` - Execute agent action
  ```json
  {
    "action": "chat",
    "params": {
      "message": "Hello, agent!"
    }
  }
  ```

### Examples

- `GET /api/examples/:type` - List examples by type (distributed/streaming/composition)
- `POST /api/examples/:type` - Run an example
  ```json
  {
    "example": "research_team"
  }
  ```

### WebSocket Events

Connect to `/ws` for real-time communication:

- **Client -> Server**:
  ```json
  {
    "type": "create_stream",
    "agent_id": "agent-uuid"
  }
  ```
  
  ```json
  {
    "type": "stream_chat",
    "message": "Your message here"
  }
  ```

- **Server -> Client**:
  ```json
  {
    "type": "stream_token",
    "token": "response token"
  }
  ```
  
  ```json
  {
    "type": "agent_event",
    "event": { ... }
  }
  ```

## Example Demonstrations

### Distributed Examples
- **Cluster**: Multi-node agent deployment
- **Research Team**: Collaborative AI research agents
- **Data Pipeline**: Distributed stream processing
- **Swarm Intelligence**: Optimization algorithms

### Streaming Examples
- **Pipeline**: Real-time data streaming with backpressure
- **Real-time Chat**: Streaming AI responses
- **Batch Processor**: Parallel batch processing
- **Event System**: Event-driven agent network

### Tool Composition Examples
- **Code Analysis**: Multi-stage analysis pipeline
- **Auto Debugging**: Autonomous debugging system
- **Security Audit**: Progressive security analysis
- **Infrastructure**: Automated deployment

## Configuration

The web application can be configured through environment variables or `sys.config`:

```erlang
{agent_web, [
  {port, 8080},
  {static_dir, "priv/static"},
  {templates_dir, "priv/templates"}
]}
```

## Development

### Adding New Handlers

1. Create a new handler module in `apps/agent_web/src/`
2. Add the route in `agent_web_sup.erl`
3. Implement the Cowboy handler callbacks

### Extending the Frontend

1. Modify `apps/agent_web/priv/static/js/app.js`
2. Add new styles in `apps/agent_web/priv/static/css/style.css`
3. The application auto-reloads on file changes

### Testing

Run the test suite:
```bash
./rebar3 eunit
```

## Troubleshooting

### Port Already in Use
If port 8080 is already in use, modify the port in `agent_web.app.src` or set the environment variable:
```bash
PORT=8081 ./start_web.sh
```

### WebSocket Connection Issues
Ensure your firewall allows WebSocket connections on the configured port.

### Agent Creation Failures
Check the console output for detailed error messages. Common issues:
- Missing OpenAI API key for AI agents
- Invalid tool specifications
- Name conflicts with existing agents

## License

MIT License - See LICENSE file for details