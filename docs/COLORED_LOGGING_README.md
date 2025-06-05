# Colored Logging System for Agents.erl

This document describes the comprehensive colored logging system implemented for the Agents.erl project.

## Overview

The colored logging system enhances log readability by applying ANSI color codes to different types of log messages, making it easier to identify errors, warnings, successes, and other important events in the system logs.

## Color Scheme

| Color | Usage | Examples |
|-------|-------|----------|
| 🔴 **Red** | Errors, failures, crashes | `ERROR`, `failed`, `crash`, `exception` |
| 🟡 **Yellow** | Warnings | `WARNING`, `warn` |
| 🟢 **Green** | Success messages, startup events | `SUCCESS`, `started`, `complete`, `ready` |
| 🔵 **Blue** | Health checks, monitoring | `HEALTH`, `check`, `monitor`, `ping` |
| 🔷 **Cyan** | API calls, HTTP requests | `API`, `GET`, `POST`, `HTTP`, `request` |
| 🟣 **Magenta** | Critical system events | `CRITICAL`, `urgent`, `alert` |
| **Bold** | Log tags and brackets | `[APP]`, `[SUP]`, `[MCP_LOG]` |

## Components

### 1. colored_logger.erl
A simple, lightweight logging module that provides colored output functions:

```erlang
% Basic usage
colored_logger:error("Database connection failed: ~p", [Reason]),
colored_logger:success("Server started on port ~p", [Port]),
colored_logger:health_check("System health check passed", []),
colored_logger:api_call("GET", "/api/users", 150),  % 150ms duration
```

**Functions:**
- `error/2`, `warning/2`, `success/2`, `info/2`, `debug/2`
- `health_check/2`, `api_call/3`, `critical/2`, `startup/2`
- `log/3`, `log/4` for custom logging

### 2. enhanced_logger.erl
A full-featured gen_server-based logging system with:
- Structured logging with metadata
- Log rotation and file management
- Performance statistics
- Configurable log levels
- Both console and file output

```erlang
% Advanced usage with components and metadata
enhanced_logger:log(error, "DATABASE", "Connection timeout", [], #{retry_count => 3}),
enhanced_logger:api_call("API_GATEWAY", "POST", "/users", 250),
```

### 3. mcp_advanced_logger.erl (Updated)
The existing MCP logger has been enhanced to use colored output for better visibility of MCP-related events.

### 4. Supervisor and Core Modules (Updated)
Key modules like `agent_web_sup.erl` and `mcp_registry.erl` have been updated to use colored logging.

## Scripts and Tools

### view_colored_logs.sh
A shell script for viewing log files with colors applied:

```bash
# View entire log file with colors
./view_colored_logs.sh view server.log

# Tail log file with colors (default: 50 lines)
./view_colored_logs.sh tail server.log

# Quick access to common logs
./view_colored_logs.sh server
./view_colored_logs.sh web
./view_colored_logs.sh app

# View all log files
./view_colored_logs.sh all
```

**Features:**
- Real-time log tailing with colors
- Pattern matching for different log levels
- Color legend for reference
- Handles multiple log file formats

### apply_colored_logging.sh
A script to systematically update existing logging calls throughout the codebase:

```bash
./apply_colored_logging.sh
```

**Features:**
- Automatically converts `io:format` calls to colored equivalents
- Creates backups of original files
- Provides summary of changes
- Includes test module for verification

## Usage Examples

### Basic Logging
```erlang
% Simple colored output
colored_logger:error("Failed to connect to database", []),
colored_logger:success("Server started successfully", []),
colored_logger:warning("Configuration file not found, using defaults", []),
```

### Component-based Logging
```erlang
% Log with component tags
colored_logger:log(info, "WEB_SERVER", "Handling request: ~s", [Path]),
colored_logger:log(error, "DATABASE", "Query failed: ~p", [Error]),
```

### API Call Logging
```erlang
% Track API performance
colored_logger:api_call("GET", "/api/users", 125),  % 125ms
colored_logger:api_call("POST", "/api/orders", 350), % 350ms
```

### Health Check Logging
```erlang
% System monitoring
colored_logger:health_check("Memory usage: ~p MB", [MemoryMB]),
colored_logger:health_check("All services operational", []),
```

## Log File Structure

The system generates several log files:
- `server.log` - Main server events and system logs
- `web.log` - Web server and HTTP request logs  
- `app.log` - Application-specific logs
- `web_server.log` - Detailed web server logs
- `startup.log` - System startup and initialization logs

## Configuration

### Log Levels
The enhanced logger supports configurable log levels:
- `debug` - Detailed debugging information
- `info` - General information messages
- `warning` - Warning conditions
- `error` - Error conditions
- `critical` - Critical system failures

### Environment Variables
Configure logging behavior via application environment:

```erlang
% In sys.config
{agent_web, [
    {log_file, "server.log"},
    {log_level, info},
    {max_log_file_size, 10485760},  % 10MB
    {max_log_files, 5}
]}
```

## Integration with Existing Code

The colored logging system is designed to be minimally invasive:

1. **Drop-in replacement**: `colored_logger` functions can replace most `io:format` calls
2. **Backward compatible**: Existing logging continues to work
3. **Gradual adoption**: Can be implemented incrementally across the codebase

## Performance Considerations

- Color codes add minimal overhead (~50 bytes per log line)
- Enhanced logger includes async processing to avoid blocking
- Log rotation prevents disk space issues
- Configurable log levels reduce output in production

## Viewing Logs

### Terminal Viewing
```bash
# View with colors in terminal
./view_colored_logs.sh server

# Tail logs in real-time
./view_colored_logs.sh tail server.log 100
```

### Production Environments
For production systems without color support:
- Set `TERM=dumb` to disable colors
- Use log aggregation tools that strip ANSI codes
- Configure file-only logging without console output

## Benefits

1. **Improved Readability**: Colors make it easier to scan logs for specific types of events
2. **Faster Debugging**: Errors and warnings stand out visually
3. **Better Monitoring**: Health checks and system events are clearly marked
4. **Enhanced Development**: Startup and success messages provide clear feedback
5. **API Tracking**: HTTP requests and API calls are easily identifiable

## Future Enhancements

Potential improvements for the colored logging system:
- JSON structured logging option
- Integration with external log aggregation services
- Custom color schemes per environment
- Log filtering and search capabilities
- Real-time log dashboard with color coding
- Integration with monitoring and alerting systems

## Testing

Test the colored logging system:

```bash
# Create and run the test module
erl -pa apps/*/ebin -eval "test_colored_logging:test()." -s init stop

# View test output in logs
./view_colored_logs.sh tail server.log
```

This comprehensive colored logging system significantly improves the developer and operator experience when working with the Agents.erl system logs.