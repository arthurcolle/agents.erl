# Pipedream MCP Integration

This integration adds Pipedream's Model Context Protocol (MCP) server to agents.erl, providing access to 2,700+ APIs and 10,000+ tools through a unified interface.

## Features

- **Autodiscovery**: Automatically discovers available Pipedream apps and their tools
- **User Authentication**: Built-in OAuth flows for connecting user accounts
- **Tool Integration**: Seamlessly integrates Pipedream tools with the existing agent tools framework
- **Web UI**: React-based interface for managing connections and discovering tools
- **Real-time Updates**: Live synchronization of user connections and available tools

## Architecture

### Core Components

1. **pipedream_mcp_client.erl** - Core MCP client for communicating with Pipedream's remote server
2. **pipedream_autodiscovery.erl** - Automatic discovery and caching of available apps and tools
3. **pipedream_auth_handler.erl** - OAuth authentication and connection management
4. **pipedream_tool_integration.erl** - Integration with agents.erl's agent tools framework
5. **pipedream_api_handler.erl** - REST API endpoints for web interface
6. **PipedreamConnectionManager.tsx** - React component for user interface

### Data Flow

```
User Request → Agent Tools Framework → Pipedream Tool Integration → MCP Client → Pipedream API
                                                                                        ↓
Tool Results ← Agent Response ← Tool Integration ← MCP Response ← Pipedream Server
```

## Setup

### 1. Environment Variables

Set the following environment variables:

```bash
export PIPEDREAM_CLIENT_ID="your_client_id"
export PIPEDREAM_CLIENT_SECRET="your_client_secret"
export PIPEDREAM_PROJECT_ID="proj_xxxxxxxxxxxxxxxx"
export PIPEDREAM_ENVIRONMENT="development"  # or "production"
```

### 2. Get Pipedream Credentials

1. Sign up at [pipedream.com](https://pipedream.com)
2. Create a new project in the Pipedream Connect dashboard
3. Generate OAuth credentials for your project
4. Copy the client ID, client secret, and project ID

### 3. Start the System

The Pipedream integration will automatically start with the main application:

```bash
make compile
./scripts/start_web.sh
```

## Usage

### Web Interface

Navigate to the web interface and access the Pipedream section to:

- Browse available apps (Gmail, Slack, Notion, etc.)
- Connect user accounts through OAuth flows
- View available tools for connected apps
- Monitor connection status and statistics

### API Endpoints

#### Discovery
- `GET /api/pipedream/apps` - List all available apps
- `GET /api/pipedream/apps/{app_slug}/tools` - Get tools for specific app
- `GET /api/pipedream/discover` - Get discovery statistics

#### User Connections
- `POST /api/pipedream/connect` - Generate connection URL for app
- `GET /api/pipedream/connections` - Get user's connected apps
- `POST /api/pipedream/disconnect` - Disconnect from an app
- `POST /api/pipedream/refresh` - Refresh connection status

#### Tool Execution
- `GET /api/pipedream/tools` - Get user's available tools
- `POST /api/pipedream/tools/call` - Execute a tool

### Agent Integration

Pipedream tools are automatically available to agents once users connect their accounts:

```erlang
% Run an agent with access to Pipedream tools
agent:run_agent("Send an email via Gmail", ["gmail_send_email"], #{
    user_id => "user123"
}).
```

Tools are namespaced by app to avoid conflicts:
- `gmail_send_email` - Send email via Gmail
- `slack_post_message` - Post message to Slack
- `notion_create_page` - Create page in Notion

## Configuration

### Tool Modes

**Sub-agent Mode** (default):
- Pipedream handles tool configuration with AI
- Simpler integration but less control
- Pass instructions as natural language

**Tools-only Mode**:
- Full control over tool parameters
- Requires handling dynamic properties
- More complex but more powerful

Set mode via environment variable:
```bash
export PIPEDREAM_TOOL_MODE="tools-only"
```

### Autodiscovery

The system automatically discovers available apps and tools. Configure discovery behavior:

```erlang
% Refresh discovery manually
pipedream_autodiscovery:refresh_discovery().

% Get discovery statistics
pipedream_autodiscovery:get_discovery_stats().
```

### Rate Limiting

The integration respects Pipedream's rate limits:
- Built-in request throttling
- Automatic retry with exponential backoff
- Connection pooling for efficiency

## Popular Apps Supported

The integration prioritizes discovery of commonly used apps:

- **Communication**: Gmail, Slack, Discord, Microsoft Teams, Zoom
- **Productivity**: Notion, Airtable, Trello, Asana, Linear, Jira
- **Business**: HubSpot, Salesforce, Stripe, Shopify, Mailchimp
- **Development**: GitHub, GitLab, Vercel, Netlify
- **Storage**: Google Drive, Dropbox, OneDrive
- **Scheduling**: Calendly, Google Calendar, Outlook

## Error Handling

The integration includes comprehensive error handling:

- Connection failures with automatic retry
- OAuth flow errors with user-friendly messages
- Tool execution timeouts and error recovery
- Rate limit handling with backoff strategies

## Monitoring

Monitor the integration through:

### Statistics API
```bash
curl http://localhost:8080/api/pipedream/stats
```

### Logs
The system provides detailed logging for:
- Tool discovery and registration
- User authentication flows
- Tool execution performance
- Error conditions and recovery

### Health Checks
```bash
curl http://localhost:8080/api/system/health
```

## Security

- OAuth credentials are stored securely
- User tokens are encrypted at rest
- All API communication uses HTTPS
- Rate limiting prevents abuse
- Audit logging for security monitoring

## Development

### Adding New Features

1. **New Tool Support**: Tools are discovered automatically from Pipedream
2. **Custom Handlers**: Extend `pipedream_tool_integration.erl` for custom behavior
3. **UI Enhancements**: Modify `PipedreamConnectionManager.tsx`

### Testing

```bash
# Test basic functionality
curl -X POST http://localhost:8080/api/pipedream/discovery/refresh

# Test user connections (requires user ID header)
curl -H "X-User-Id: test_user" http://localhost:8080/api/pipedream/connections
```

### Debugging

Enable detailed logging:
```bash
export PD_SDK_DEBUG=true
export PIPEDREAM_LOG_RESPONSES=true
```

## Troubleshooting

### Common Issues

**"Client ID not found"**
- Check environment variables are set correctly
- Verify Pipedream project credentials

**"Tools not appearing"**
- Ensure user has connected the required app
- Check app supports the required actions
- Verify autodiscovery is running

**"Connection failed"**
- Check internet connectivity
- Verify Pipedream service status
- Review API rate limits

**"Tool execution timeout"**
- Increase timeout in configuration
- Check tool complexity and requirements
- Verify user permissions for the action

### Support

For issues specific to:
- **Pipedream API**: Check [Pipedream documentation](https://pipedream.com/docs)
- **agents.erl integration**: Check application logs and error tracking
- **OAuth flows**: Verify redirect URLs and credentials

## Roadmap

Future enhancements planned:

- **Workflow Support**: Integration with Pipedream workflows
- **Bulk Operations**: Execute multiple tools in sequence
- **Advanced Filtering**: Smart tool discovery based on user context
- **Performance Optimization**: Caching and connection pooling improvements
- **Enterprise Features**: Team management and permissions

## Contributing

When contributing to the Pipedream integration:

1. Follow Erlang/OTP conventions
2. Add appropriate error handling
3. Include logging for debugging
4. Update documentation for new features
5. Test with multiple apps and scenarios

## License

This integration follows the same license as the main agents.erl project.