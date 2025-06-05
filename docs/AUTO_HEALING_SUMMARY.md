# Auto-Healing System for agents.erl

## Overview

I've created a comprehensive auto-healing system that automatically detects and fixes common errors in your Erlang agent system. The system consists of three main components:

## 1. Self-Healing Supervisor (`self_healing_supervisor.erl`)

A smart supervisor that monitors critical processes and automatically restarts them when they fail.

**Features:**
- Monitors process health with configurable health checks
- Automatically restarts failed processes
- Pattern-based error detection and fixing
- Circuit breaker pattern to prevent restart loops
- Configurable retry limits

**Usage:**
```erlang
%% Add a process to monitoring
self_healing_supervisor:add_monitor(ProcessName, Module, #{
    health_check => fun(_) -> ok end,
    auto_fix => fun(Name, Error) -> Module:start_link() end,
    max_retries => 3
}).
```

## 2. Error Resilience Module (`error_resilience.erl`)

Provides resilient wrappers for common operations that can fail.

**Features:**
- **Safe gen_server calls** - Returns default values instead of crashing
- **Protected division** - Prevents badarith errors from division by zero
- **Memory calculation protection** - Safe memory percentage calculations
- **Retry mechanism** - Automatic retry with exponential backoff
- **Circuit breaker** - Prevents cascading failures
- **Fallback values** - Returns sensible defaults when operations fail

**Key Functions:**
```erlang
%% Safe gen_server call with default value
error_resilience:safe_call(Server, Request, Timeout, DefaultValue)

%% Protected division (no more badarith!)
error_resilience:protect_division(Numerator, Denominator)

%% Safe memory calculation
error_resilience:protect_memory_calc(Used, Total)

%% Execute with retry
error_resilience:with_retry(Fun, MaxRetries, Delay)

%% Circuit breaker pattern
error_resilience:with_circuit_breaker(Name, Fun, Options)
```

## 3. Auto-Healing Startup (`auto_healing_startup.erl`)

Ensures all critical processes are running and continuously monitors system health.

**Features:**
- Automatically starts missing processes
- Continuous health monitoring
- Automatic MCP server reconnection
- Memory usage monitoring and garbage collection
- Dead process detection and restart

**Usage:**
```erlang
%% Start the entire system with auto-healing
auto_healing_startup:start_with_healing()

%% Ensure all systems are running
auto_healing_startup:ensure_all_systems_running()
```

## Fixed Issues

### 1. **badarith in mcp_monitor**
- **Problem**: Division by zero when calculating memory usage
- **Fix**: Added check for TotalMem = 0 and integrated error_resilience module

### 2. **noproc for mcp_orchestration_engine**
- **Problem**: Process not started by supervisor
- **Fix**: Added to supervisor child specs

### 3. **MCP Server Connection Failures**
- **Problem**: All servers showing error status
- **Fix**: Auto-reconnection logic in monitor_and_fix/0

## Quick Start

1. **Apply the fixes:**
   ```bash
   ./apply_auto_fixes.sh
   ```

2. **Start with auto-healing:**
   ```bash
   ./start_auto_healing.erl
   ```

## How It Works

The auto-healing system operates on multiple levels:

1. **Prevention**: Wraps dangerous operations (like division) in safe functions
2. **Detection**: Continuously monitors process health and system metrics
3. **Recovery**: Automatically restarts failed processes and reconnects servers
4. **Learning**: Tracks failure patterns to prevent future issues

## Benefits

- **Zero-downtime**: Processes are automatically restarted
- **Self-correcting**: Common errors are fixed automatically
- **Resilient**: System continues operating even with failures
- **Observable**: All actions are logged for debugging

## Monitoring

The system logs all auto-healing actions:
- `[AUTO_HEALING]` - General healing actions
- `[RESILIENCE]` - Error prevention and fallbacks
- `[SELF_HEALING]` - Process monitoring and recovery

## Future Enhancements

The system can be extended with:
- Machine learning for predictive failure detection
- Automatic code patching for recurring errors
- Distributed healing across clusters
- Performance optimization based on patterns