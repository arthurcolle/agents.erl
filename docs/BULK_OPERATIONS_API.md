# Bulk Operations API Documentation

The Distributed Agent System now supports powerful bulk operations for managing multiple agents simultaneously. These endpoints enable mass operations like broadcasting messages, modifying system prompts, and executing custom callbacks across agent fleets.

## Available Endpoints

### 1. Bulk Broadcast (`/api/bulk/broadcast`)

Send a message to multiple agents simultaneously with different execution modes.

**Method:** POST

**Request Body:**
```json
{
  "message": "Your message to broadcast to agents",
  "filters": {
    "type": "template",
    "model": "gpt-4o",
    "template_id": "researcher",
    "status": "active",
    "name_contains": "Assistant"
  },
  "options": {
    "execution_mode": "parallel",
    "timeout": 30000
  }
}
```

**Execution Modes:**
- `parallel` - Execute all agent calls simultaneously (fastest)
- `sequential` - Execute agent calls one by one (slower but more controlled)
- `fire_and_forget` - Start execution and return immediately

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/broadcast \
  -H "Content-Type: application/json" \
  -d '{
    "message": "What is your current status and capabilities?",
    "options": {
      "execution_mode": "parallel",
      "timeout": 15000
    }
  }'
```

### 2. System Prompt Modification (`/api/bulk/system-prompt`)

Modify system prompts for multiple agents using prefix, suffix, or replacement operations.

**Method:** POST

**Request Body:**
```json
{
  "operation": "add_prefix",
  "text": "You are now operating in debug mode. ",
  "filters": {
    "type": "template",
    "template_id": "debugger"
  }
}
```

**Operations:**
- `add_prefix` - Add text to the beginning of system prompts
- `add_suffix` - Add text to the end of system prompts
- `replace` - Replace entire system prompt
- `reset` - Reset to original template system prompt

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/system-prompt \
  -H "Content-Type: application/json" \
  -d '{
    "operation": "add_prefix",
    "text": "URGENT: You are now in emergency response mode. ",
    "filters": {
      "type": "template"
    }
  }'
```

### 3. Callback Operations (`/api/bulk/callback`)

Execute functional programming operations (map, filter, reduce, foreach, custom) on agents.

**Method:** POST

**Request Body:**
```json
{
  "callback_type": "map",
  "code": "return_agent_type",
  "parameters": {
    "additional_data": "value"
  },
  "filters": {
    "status": "active"
  }
}
```

**Callback Types:**
- `map` - Transform each agent and return results
- `filter` - Filter agents based on criteria
- `reduce` - Reduce agents to a single value
- `foreach` - Execute operation on each agent
- `custom` - Execute predefined custom operations

**Available Custom Codes:**
- `get_memory_usage` - Get memory usage for all agents
- `get_message_queue_length` - Get message queue lengths
- `return_agent_id` - Return agent IDs
- `return_agent_type` - Return agent types
- `check_memory_usage` - Check if memory exceeds threshold

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/callback \
  -H "Content-Type: application/json" \
  -d '{
    "callback_type": "custom",
    "code": "get_memory_usage"
  }'
```

### 4. Agent Management (`/api/bulk/agent-management`)

Batch management operations for agent lifecycle and configuration.

**Method:** POST

**Request Body:**
```json
{
  "operation": "get_status",
  "filters": {
    "type": "template"
  },
  "parameters": {
    "new_config": "value"
  }
}
```

**Operations:**
- `stop` - Stop selected agents
- `restart` - Restart selected agents
- `update_config` - Update agent configurations
- `get_status` - Get status information
- `get_metrics` - Get performance metrics
- `clear_history` - Clear conversation histories

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/agent-management \
  -H "Content-Type: application/json" \
  -d '{
    "operation": "get_metrics",
    "filters": {
      "model": "gpt-4o"
    }
  }'
```

### 5. Bulk Query (`/api/bulk/query`)

Query information from multiple agents efficiently.

**Method:** POST

**Request Body:**
```json
{
  "query_type": "agent_info",
  "filters": {
    "status": "active"
  },
  "parameters": {
    "limit": 10
  }
}
```

**Query Types:**
- `agent_info` - Get detailed agent information
- `conversation_history` - Get conversation histories
- `performance_metrics` - Get performance data
- `capabilities` - Get agent capabilities
- `health_check` - Get health status

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/query \
  -H "Content-Type: application/json" \
  -d '{
    "query_type": "health_check"
  }'
```

### 6. Data Transformation (`/api/bulk/transform`)

Transform and analyze data using built-in operations.

**Method:** POST

**Request Body:**
```json
{
  "transform_type": "aggregate",
  "data": [
    {"value": 10, "category": "A"},
    {"value": 20, "category": "B"}
  ],
  "parameters": {
    "aggregation_type": "sum",
    "field": "value"
  }
}
```

**Transform Types:**
- `aggregate` - Aggregate data (count, sum, average)
- `normalize` - Normalize numerical data
- `analyze` - Analyze data structure and types
- `format` - Format data (json, csv)

**Example Usage:**
```bash
curl -X POST http://localhost:8080/api/bulk/transform \
  -H "Content-Type: application/json" \
  -d '{
    "transform_type": "analyze",
    "data": [{"name": "agent1", "memory": 100}, {"name": "agent2", "memory": 200}]
  }'
```

## Filtering

All endpoints support powerful filtering to target specific agents:

```json
{
  "filters": {
    "type": "template",           // Agent type
    "model": "gpt-4o",           // AI model
    "template_id": "researcher",  // Template ID
    "status": "active",          // Agent status
    "name_contains": "Assistant" // Name substring match
  }
}
```

## Response Format

All bulk operations return a consistent response format:

```json
{
  "success": true,
  "operation": "broadcast",
  "agents_targeted": 5,
  "execution_mode": "parallel",
  "results": [
    {
      "agent_id": "abc123",
      "result": {
        "ok": "response data"
      }
    }
  ]
}
```

## Error Handling

- **400 Bad Request**: Invalid JSON or missing required fields
- **404 Not Found**: Unknown endpoint
- **405 Method Not Allowed**: Only POST requests supported
- **500 Internal Server Error**: Agent execution errors

## Performance Considerations

- Use `parallel` execution mode for speed with many agents
- Use `sequential` for more controlled execution
- Use `fire_and_forget` for background operations
- Filter agents to reduce processing overhead
- Set appropriate timeouts for long-running operations

## Lambda/Callback Programming

The callback system supports functional programming patterns:

```javascript
// Example: Get high-memory agents
{
  "callback_type": "filter",
  "code": "check_memory_usage",
  "parameters": {
    "threshold": 50000000  // 50MB
  }
}

// Example: Count active agents by type
{
  "callback_type": "reduce",
  "code": "count_by_type",
  "parameters": {
    "initial_value": {}
  }
}
```

## Use Cases

1. **Fleet Management**: Monitor and control large agent deployments
2. **Emergency Response**: Quickly reconfigure all agents for urgent tasks  
3. **A/B Testing**: Compare different configurations across agent groups
4. **Performance Monitoring**: Collect metrics from all agents simultaneously
5. **Batch Updates**: Update system prompts or configurations en masse
6. **Health Checks**: Monitor system-wide agent health and performance
7. **Load Balancing**: Distribute work across available agents
8. **Debugging**: Gather diagnostic information from multiple agents

The bulk operations system provides unprecedented control and visibility into your distributed agent fleet, enabling powerful automation and management workflows.