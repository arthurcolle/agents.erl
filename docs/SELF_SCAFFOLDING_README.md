# Self-Scaffolding System

The self-scaffolding system automatically discovers and integrates API endpoints from OpenAPI specifications, with hourly updates to ensure the system stays current with API changes.

## Architecture

The system consists of several key components:

### 1. **Self-Scaffold Supervisor** (`self_scaffold_sup.erl`)
- Manages all self-scaffolding components
- Ensures fault tolerance and automatic recovery
- Supervises: endpoint registry, OpenAPI fetcher, endpoint discovery, and scheduler

### 2. **Endpoint Registry** (`endpoint_registry.erl`)
- Stores discovered API endpoints with metadata
- Provides fast lookups using ETS tables
- Persists data to SQLite for durability
- Tracks request logs and usage metrics

### 3. **OpenAPI Fetcher** (`openapi_fetcher.erl`)
- Fetches OpenAPI/Swagger specifications from various sources
- Parses YAML/JSON schemas into Erlang structures
- Caches schemas for offline access
- Supports multiple API providers (OpenAI, Anthropic, GitHub, etc.)

### 4. **Endpoint Discovery** (`endpoint_discovery.erl`)
- Extracts endpoint definitions from OpenAPI schemas
- Registers discovered endpoints automatically
- Runs discovery on a configurable schedule (default: hourly)
- Supports manual and automatic discovery modes

### 5. **Scaffold Scheduler** (`scaffold_scheduler.erl`)
- Manages scheduled updates and discovery tasks
- Ensures endpoints are refreshed hourly
- Provides manual override capabilities
- Tracks update history and metrics

## Features

- **Automatic Discovery**: Discovers API endpoints from OpenAPI specifications
- **Hourly Updates**: Automatically refreshes endpoint information every hour
- **Multi-Source Support**: Works with OpenAI, Anthropic, GitHub, and custom APIs
- **Persistent Storage**: Uses SQLite to persist endpoint data across restarts
- **Request Logging**: Tracks API usage for analytics and debugging
- **Agent Integration**: Provides tools for agents to use discovered endpoints

## Usage

### Starting the System

The self-scaffolding system starts automatically with the agent_web application:

```erlang
% Start the application
application:ensure_all_started(agent_web).
```

### Manual Discovery

Trigger endpoint discovery manually:

```erlang
% Discover from a specific source
{ok, Count} = endpoint_discovery:discover_endpoints(openai).

% Discover from all configured sources
{ok, Results, Total} = endpoint_discovery:discover_all_endpoints().
```

### Checking Status

```erlang
% Get discovery status
Status = endpoint_discovery:get_discovery_status().

% Get scheduler information
Info = scaffold_scheduler:get_schedule_info().
```

### Working with Endpoints

```erlang
% Get all registered endpoints
Endpoints = endpoint_registry:get_all_endpoints().

% Get endpoints by tag
ChatEndpoints = endpoint_registry:get_endpoints_by_tag(<<"chat">>).

% Get specific endpoint
{ok, Endpoint} = endpoint_registry:get_endpoint(<<"POST">>, <<"/v1/chat/completions">>).
```

### Agent Integration

Agents can use the discovered endpoints through the API tools:

```erlang
% Create an agent with API tools
Config = #{
    name => <<"API Explorer">>,
    model => <<"gpt-4.1-mini">>,
    tools => [api_discovery, api_testing]
},
{ok, AgentId} = agent:create(Config).

% Agent can discover and use APIs
agent:execute(AgentId, #{
    action => <<"chat">>,
    message => <<"List available OpenAI endpoints for embeddings">>
}).
```

## Configuration

### Update Interval

Change the update interval (default: 1 hour):

```erlang
% Set to 30 minutes (in milliseconds)
scaffold_scheduler:set_update_interval(1800000).
endpoint_discovery:set_discovery_interval(1800000).
```

### Add Custom API Sources

```erlang
% Add a custom OpenAPI source
openapi_fetcher:add_schema_source(my_api, "https://api.example.com/openapi.yaml").
```

## Database Schema

The system uses SQLite with the following tables:

### endpoints
- `method`: HTTP method (GET, POST, etc.)
- `path`: API endpoint path
- `operation_id`: Unique operation identifier
- `summary`: Endpoint description
- `tag`: API category/tag
- `parameters`: JSON-encoded parameters
- `request_schema`: JSON-encoded request schema
- `response_schema`: JSON-encoded response schema
- `last_updated`: Timestamp of last update

### request_logs
- `timestamp`: Request timestamp
- `endpoint`: Endpoint path
- `method`: HTTP method
- `request_payload`: JSON request data
- `response_payload`: JSON response data
- `cost_estimate`: Estimated API cost
- `prompt_tokens`: Token usage
- `completion_tokens`: Token usage

## Running the Demo

```bash
# Compile and run the demo
erlc -o ebin examples/self_scaffold_demo.erl
erl -pa ebin -pa apps/*/ebin -s self_scaffold_demo run -s init stop
```

## Monitoring

The system logs all discovery activities and updates:

```erlang
% View logs in the Erlang shell
error_logger:tty(true).

% Check recent request logs
% (These are stored in the request_logs ETS table and SQLite database)
```

## Troubleshooting

1. **Discovery Fails**: Check network connectivity and API URLs
2. **No Endpoints Found**: Verify OpenAPI schema format and parsing
3. **Updates Not Running**: Check scheduler status and timer configuration
4. **Database Errors**: Ensure write permissions for SQLite database file

## Future Enhancements

- Dynamic route generation for discovered endpoints
- Automatic SDK generation for agents
- Rate limiting and quota management
- GraphQL schema support
- WebSocket endpoint discovery
- API versioning and migration support