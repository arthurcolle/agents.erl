# Comprehensive Logging Guide for Erlang Agent System

This guide documents the extensive logging capabilities added to the multi-turn function calling system for improved debugging and monitoring.

## Table of Contents
1. [Overview](#overview)
2. [Logging Prefixes](#logging-prefixes)
3. [Log Levels and Colors](#log-levels-and-colors)
4. [Module-Specific Logging](#module-specific-logging)
5. [Log Analysis Tips](#log-analysis-tips)
6. [Performance Monitoring](#performance-monitoring)
7. [Troubleshooting Guide](#troubleshooting-guide)

## Overview

The enhanced logging system provides detailed insights into:
- Multi-turn conversation flow
- Tool execution lifecycle
- API request/response details
- Streaming operations
- Performance metrics
- Error diagnostics

## Logging Prefixes

Each log message is prefixed with a category identifier for easy filtering:

| Prefix | Module | Description |
|--------|--------|-------------|
| `[AGENT_EXEC]` | agent_instance | Agent execution lifecycle |
| `[CHAT]` | agent_instance | Chat processing flow |
| `[MULTI_TURN]` | agent_instance | Multi-turn execution tracking |
| `[TOOL_EXEC]` | agent_instance | Tool execution coordination |
| `[TOOL_MSG]` | agent_instance | Tool message creation |
| `[STREAM_FUNC]` | streaming_function_handler | Streaming function calls |
| `[TOOLS]` | agent_tools | Tool registry operations |
| `[TOOLS_EXEC]` | agent_tools | Individual tool execution |
| `[PREDEFINED]` | agent_tools | Predefined tool execution |
| `[SHELL]` | agent_tools | Shell command execution |
| `[FILE]` | agent_tools | File operations |
| `[CHAT_API]` | openai_chat | OpenAI Chat API calls |
| `[CHAT_STREAM]` | openai_chat | Chat streaming operations |
| `[RESPONSES_API]` | openai_responses | OpenAI Responses API calls |

## Log Levels and Colors

The system uses colored logging for visual clarity:

- 🟢 **SUCCESS** (Green) - Successful operations
- 🔵 **INFO** (Blue) - General information
- 🟡 **WARNING** (Yellow) - Warning conditions
- 🔴 **ERROR** (Red) - Error conditions
- ⚫ **DEBUG** (Gray) - Detailed debug information

## Module-Specific Logging

### agent_instance.erl

#### Chat Processing
```erlang
[CHAT] ╔════════════════════════════════════════╗
[CHAT] ║        CHAT PROCESSING START           ║
[CHAT] ╚════════════════════════════════════════╝
[CHAT] 💬 Message preview: What's the weather...
[CHAT] 📏 Message size: 45 bytes
[CHAT] 🆔 Agent ID: <<"weather_agent">>
[CHAT] 🤖 Model: <<"gpt-4o-mini">>
[CHAT] 🔧 Available tools: [jina_search, file_read]
[CHAT] 📚 Conversation history: 3 messages
[CHAT] 📊 Max history: 50
[CHAT] 🌐 API preference: responses_api
```

#### Multi-Turn Execution
```erlang
[MULTI_TURN] ╔════════════════════════════════════════╗
[MULTI_TURN] ║     MULTI-TURN EXECUTION START         ║
[MULTI_TURN] ╚════════════════════════════════════════╝
[MULTI_TURN] 📊 Execution Depth: 1/5
[MULTI_TURN] 🔧 Number of tool calls: 2
[MULTI_TURN] ⏰ Turn start time: 2025-06-03 00:15:42
[MULTI_TURN] 📝 Conversation context: 4 messages
[MULTI_TURN] 🎯 Tool calls breakdown:
[MULTI_TURN]   [1] 🔧 jina_search (id: call_abc123) args: {"query":"weather in SF"}
[MULTI_TURN]   [2] 🔧 jina_search (id: call_def456) args: {"query":"weather in NYC"}
```

#### Tool Execution Results
```erlang
[MULTI_TURN] ✅ Tool execution completed in 1523ms
[MULTI_TURN] 📊 Tool results summary:
[MULTI_TURN]   [1] ✅ call_abc123: SUCCESS (2048 bytes)
[MULTI_TURN]   [2] ✅ call_def456: SUCCESS (1856 bytes)
```

### agent_tools.erl

#### Tool Execution Lifecycle
```erlang
[TOOLS_EXEC] ╔════════════════════════════════════════╗
[TOOLS_EXEC] ║      TOOL EXECUTION START              ║
[TOOLS_EXEC] ╚════════════════════════════════════════╝
[TOOLS_EXEC] 🔧 Tool: jina_search
[TOOLS_EXEC] 👤 From: {<0.123.0>, #Ref<0.456.789>}
[TOOLS_EXEC] 📊 Arguments: #{<<"query">> => <<"weather forecast">>}
[TOOLS_EXEC] ✅ Found local executor for jina_search
[TOOLS_EXEC] 🚀 Invoking executor...
[TOOLS_EXEC] ✅ Execution success (523ms) - Result: 2048 bytes
[TOOLS_EXEC] 🏁 Total execution time: 524ms
[TOOLS_EXEC] ╚════════════════════════════════════════╝
```

#### Predefined Tool Execution
```erlang
[PREDEFINED] Executing predefined tool: file_read
[PREDEFINED] 📄 Reading file: /path/to/file.txt
[FILE] Reading file: /path/to/file.txt
[FILE] Read 1024 bytes from /path/to/file.txt in 5ms
[PREDEFINED] Tool file_read completed in 6ms, result: 1024 bytes
```

### streaming_function_handler.erl

#### Streaming Events
```erlang
[STREAM_FUNC] Processing event type: tool_call_start
[STREAM_FUNC] Tool call started - Index: 0, Call: #{<<"name">> => <<"search">>}
[STREAM_FUNC] Tool call delta - Index: 0
[STREAM_FUNC] Tool call complete - Index: 0
[STREAM_FUNC] ✅ Completed tool: search
[STREAM_FUNC] 🏁 Stream done signal received
[STREAM_FUNC] Stream summary:
[STREAM_FUNC]   📝 Content: 256 bytes
[STREAM_FUNC]   ✅ Complete tools: 1
[STREAM_FUNC]   ⚠️  Partial tools: 0
```

### openai_chat.erl / openai_responses.erl

#### API Request Logging
```erlang
[CHAT_API] 📤 Chat completion request
[CHAT_API] Model: <<"gpt-4o-mini">>
[CHAT_API] Messages: 5
[CHAT_API] 🔧 Using 3 tools
[CHAT_API] 🚀 Sending request to OpenAI...
[CHAT_API] ✅ Response received in 1234ms
[CHAT_API] 📊 Token usage - Prompt: 850, Completion: 150, Total: 1000
[CHAT_API] 🔧 Response contains 2 tool calls
```

#### Error Logging
```erlang
[CHAT_API] ❌ API error after 523ms - Status: 429
[CHAT_API] Error details - Type: rate_limit_exceeded, Message: Rate limit exceeded
```

## Log Analysis Tips

### 1. Filtering by Module
```bash
# View only multi-turn execution logs
grep "\[MULTI_TURN\]" agent.log

# View only tool execution logs
grep "\[TOOL" agent.log

# View errors across all modules
grep "❌\|ERROR\|💥" agent.log
```

### 2. Performance Analysis
```bash
# Find slow tool executions (>1000ms)
grep "completed in [0-9]\{4,\}ms" agent.log

# API response times
grep "Response received in" agent.log | awk '{print $NF}'
```

### 3. Multi-Turn Flow Tracking
```bash
# Track complete multi-turn conversations
grep -A20 "MULTI-TURN EXECUTION START" agent.log
```

## Performance Monitoring

The enhanced logging provides detailed performance metrics:

1. **Execution Times**
   - Individual tool execution duration
   - API request/response times
   - Complete multi-turn cycle duration

2. **Token Usage**
   - Prompt tokens
   - Completion tokens
   - Total tokens per request

3. **Data Volumes**
   - Request/response sizes
   - Tool input/output sizes
   - Content lengths

## Troubleshooting Guide

### Common Issues and Log Patterns

#### 1. Tool Not Found
```
[TOOLS_EXEC] 🔍 Tool weather_tool not found locally, checking MCP...
[TOOLS_EXEC] ❌ Tool weather_tool not found in MCP either
```

#### 2. Multi-Turn Depth Limit
```
[MULTI_TURN] Max tool call depth (5) reached, stopping recursion
```

#### 3. Streaming Timeout
```
[STREAM_FUNC] Stream timeout after 30 seconds
```

#### 4. API Rate Limiting
```
[CHAT_API] ❌ API error after 234ms - Status: 429
[CHAT_API] Error details - Type: rate_limit_exceeded
```

### Debug Mode

For even more detailed logging, set the log level to DEBUG:
```erlang
% In your configuration
{log_level, debug}
```

This will enable:
- Full request/response bodies
- Detailed argument logging
- Complete stack traces
- Raw API responses

## Best Practices

1. **Log Rotation**: Implement log rotation to prevent disk space issues
2. **Structured Logging**: Use the prefix system for easy parsing
3. **Performance Impact**: Be aware that DEBUG level logging can impact performance
4. **Sensitive Data**: The system logs arguments and responses - ensure no sensitive data is exposed

## Future Enhancements

Planned logging improvements:
- [ ] Structured JSON logging option
- [ ] Log aggregation support
- [ ] Real-time log streaming dashboard
- [ ] Automatic performance anomaly detection
- [ ] Log sampling for high-volume scenarios

---

This comprehensive logging system provides unparalleled visibility into the multi-turn function calling flow, making debugging and performance optimization significantly easier.