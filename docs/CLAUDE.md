# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a sophisticated **Distributed Systems** framework - a distributed multi-agent system with OpenAI/Anthropic integration, quantum-inspired coordination, and modern web interface. The system combines traditional Erlang/OTP reliability with cutting-edge AI capabilities.

## Essential Commands

### Development
```bash
# Start development environment (recommended)
./scripts/start_web.sh      # Starts web interface at http://localhost:8080

# Build and compile
make compile                 # Compile all applications
./rebar3 compile            # Alternative compile command

# Interactive development
./rebar3 shell              # Erlang shell with project loaded
make shell                  # Alternative shell command

# Testing and quality
make test                   # Run EUnit tests from tests/ directory
make dialyzer              # Static analysis
./rebar3 eunit             # Run specific test suites
```

### Production
```bash
make release               # Build production release
./scripts/start_prod.sh    # Start with JIT compilation enabled
```

## Architecture Overview

### OTP Application Structure

**3-Tier Application Architecture:**

1. **`apps/agents/`** - Core agent system
   - Main process supervision and agent orchestration
   - Tool execution framework and quantum coordination
   - Entry point: `agent.erl` (application behavior)

2. **`apps/openai/`** - AI API integration layer  
   - OpenAI and Anthropic/Claude API clients
   - Rate limiting and response handling
   - Entry point: `openai_app.erl`

3. **`apps/agent_web/`** - Web interface and HTTP APIs
   - Cowboy HTTP server with WebSocket support
   - React/TypeScript frontend with real-time features
   - Entry point: `agent_web_app.erl`

### Key Components

**Agent System:**
- `agent_tools.erl` - Tool execution framework with function calling
- `quantum_protocol.erl` - Quantum-inspired process coordination  
- `cluster_orchestrator.erl` - Multi-agent orchestration
- `lockfree_coordination.erl` - High-performance coordination primitives

**API Integration:**
- `openai_chat.erl` - Chat completions with streaming support
- `anthropic_client.erl` - Claude API integration
- `openai_rate_limiter.erl` - API throttling and management

## Configuration

### Environment Setup
```bash
export OPENAI_API_KEY=your_key_here    # Required for OpenAI integration
export ANTHROPIC_API_KEY=your_key_here # Required for Claude integration
```

### Key Configuration Files
- `config/sys.config` - Application runtime configuration
- `config/vm.args.prod` - Production VM arguments and performance tuning
- `rebar.config` - Build configuration with dependency management

## Development Patterns

### OTP Principles
- All major components follow OTP supervision tree patterns
- Use `gen_server` for stateful processes (API clients, registries)
- Hot code reloading supported for runtime updates
- Fault tolerance through supervisor restart strategies

### AI Integration Patterns
- Function calling tools implemented in `agent_tools.erl`
- Streaming responses handled via WebSocket connections
- API rate limiting enforced at client level
- Multiple AI providers supported through unified interface

### Advanced Features
- **Quantum Coordination**: Process entanglement for distributed scenarios
- **Self-Optimization**: Runtime code analysis and generation
- **Swarm Intelligence**: Multiple coordination algorithms available
- **Lock-Free Primitives**: Ultra-high-performance coordination for latency-critical paths

## Testing Strategy

Run tests with:
```bash
make test                    # Full test suite
./rebar3 eunit apps/agents  # Test specific application
./rebar3 eunit --suite=agent_tools_tests  # Test specific module
```

Test files located in `tests/` and `apps/*/test/` directories following EUnit conventions.

## Web Interface

Frontend built with React/TypeScript and shadcn/ui components. Development workflow:
```bash
cd apps/agent_web/frontend
npm install
npm run dev                 # Development server
npm run build              # Production build
```

## Function Calling

Custom function calling is fully implemented and follows the OpenAI specification:

### Tool Definition Format
Tools use strict mode for reliable execution:
```erlang
#{
    <<"type">> => <<"function">>,
    <<"name">> => <<"tool_name">>,
    <<"description">> => <<"Tool description">>,
    <<"parameters">> => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{...},
        <<"required">> => [...],
        <<"additionalProperties">> => false  % Required for strict mode
    },
    <<"strict">> => true
}
```

### Function Call Handling
- Supports both new Responses API and legacy Chat API formats
- Automatic detection and parsing of function calls
- Parallel function calling supported
- Streaming function calls handled correctly

### Tool Execution
Tools are executed through `agent_tools:execute_tool/2` with automatic result formatting.

## Hot Code Reloading

The system supports live code updates without restart:

### Manual Reloading
```bash
# Reload a specific module
./scripts/hot_reload.sh reload agent_tools

# Compile and reload from source
./scripts/hot_reload.sh compile apps/agents/src/agent_tools.erl

# Reload all application modules
./scripts/hot_reload.sh reload-all

# Get module information
./scripts/hot_reload.sh info agent_tools
```

### File Watching
Automatic reloading when files change:
```bash
# Watch specific files
./scripts/hot_reload.sh watch apps/agents/src/*.erl

# Stop watching
./scripts/hot_reload.sh stop-watch
```

### Agent Tools
Hot reloading available through agent function calls:
- `reload_module` - Reload specific module
- `reload_all_modules` - Reload all changed modules
- `compile_and_reload` - Compile and reload from source
- `watch_files` - Start automatic file watching
- `get_module_info` - Inspect loaded modules

### HTTP API
Direct HTTP access at `/api/reload` with POST requests:
```json
{"action": "reload_module", "module": "agent_tools"}
{"action": "compile_and_reload", "source_file": "apps/agents/src/agent_tools.erl"}
{"action": "reload_all"}
```

## Performance Considerations

- Use `./scripts/start_prod.sh` for production deployments (enables JIT compilation)
- Monitor process memory with `:observer.start()` in shell
- Quantum coordination provides microsecond-level latency for critical paths
- Lock-free coordination available for maximum performance scenarios