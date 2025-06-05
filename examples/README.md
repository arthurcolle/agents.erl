# Advanced Distributed Systems Examples

This directory contains advanced examples demonstrating the full capabilities of the Distributed Systems framework.

## Overview

The examples are organized into three main categories:

1. **Distributed Agents** - Multi-node coordination, swarm intelligence, fault tolerance
2. **Tool Composition** - Complex tool chains, autonomous workflows, pipeline processing
3. **Streaming & Async** - Real-time processing, event-driven systems, parallel execution

## Running the Examples

### Quick Start

```bash
# From the project root
cd examples
../rebar3 shell

# Run all examples
run_advanced_examples:run_all().

# Interactive menu
run_advanced_examples:demo_menu().

# Run specific category
run_advanced_examples:run_distributed().
run_advanced_examples:run_tool_composition().
run_advanced_examples:run_streaming().
```

### Individual Examples

```erlang
% Distributed Examples
advanced_distributed_agents:start_distributed_cluster([node()]).
advanced_distributed_agents:collaborative_research(<<"Climate Change">>, [research, analysis]).
advanced_distributed_agents:agent_swarm_computation(<<"Optimize">>, 10, #{}).

% Tool Composition Examples
advanced_tool_composition:code_analysis_pipeline(<<"/path/to/project">>, #{}).
advanced_tool_composition:autonomous_debugging_session(<<"Error description">>, <<"/src">>).
advanced_tool_composition:security_audit_chain(<<"target-system">>, #{}).

% Streaming Examples
advanced_streaming_async:streaming_data_pipeline(<<"data-source">>, #{}).
advanced_streaming_async:async_agent_orchestra(<<"Task">>, [<<"Sub1">>, <<"Sub2">>], #{}).
advanced_streaming_async:real_time_chat_processor(self(), #{model => <<"gpt-4">>}).
```

## Example Categories

### 1. Distributed Agent Examples (`advanced_distributed_agents.erl`)

- **Distributed Cluster**: Multi-node agent deployment with mesh networking
- **Collaborative Research**: Multiple specialized agents working together
- **Data Processing Pipeline**: Distributed stream processing across nodes
- **Cross-Node Communication**: Inter-agent messaging across Erlang nodes
- **Agent Swarm**: Swarm intelligence for complex problem solving
- **Hierarchical Network**: Multi-level decision-making hierarchy
- **Fault Tolerance**: Automatic failover and redundancy

### 2. Tool Composition Examples (`advanced_tool_composition.erl`)

- **Code Analysis Pipeline**: Multi-stage code quality analysis
- **Autonomous Debugging**: Self-directed debugging with tool selection
- **Data Science Workflow**: Complete ML pipeline from data to deployment
- **Security Audit Chain**: Progressive security analysis and testing
- **Infrastructure Automation**: Complex deployment orchestration
- **Knowledge Extraction**: NLP pipeline for document analysis
- **Real-time Monitoring**: Adaptive monitoring with automated responses

### 3. Streaming & Async Examples (`advanced_streaming_async.erl`)

- **Streaming Pipeline**: Backpressure-aware data processing
- **Async Orchestra**: Parallel task execution with coordination
- **Real-time Chat**: Streaming chat responses with OpenAI
- **Code Generator**: Incremental code generation with streaming
- **Batch Processor**: Parallel batch processing with worker pools
- **Event System**: Reactive event-driven agent network
- **Analysis Engine**: Real-time streaming analytics

## Architecture Patterns

### Distributed Patterns

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   Node 1    │────│   Node 2    │────│   Node 3    │
│  ┌───────┐  │     │  ┌───────┐  │     │  ┌───────┐  │
│  │Agent A│  │     │  │Agent B│  │     │  │Agent C│  │
│  └───────┘  │     │  └───────┘  │     │  └───────┘  │
└─────────────┘     └─────────────┘     └─────────────┘
        ↓                   ↓                   ↓
    ┌─────────────────────────────────────────────┐
    │            Distributed Message Bus           │
    └─────────────────────────────────────────────┘
```

### Tool Composition Pipeline

```
Input → [Parse] → [Analyze] → [Transform] → [Enrich] → [Output]
          ↓          ↓           ↓            ↓          ↓
       [Tool 1]  [Tool 2,3]  [Tool 4]    [Tool 5]  [Tool 6]
```

### Streaming Architecture

```
┌──────────┐    ┌──────────┐    ┌──────────┐
│  Source  │───→│ Process  │───→│  Sink    │
└──────────┘    └──────────┘    └──────────┘
                     ↓
                ┌──────────┐
                │ Monitor  │
                └──────────┘
```

## Advanced Features Demonstrated

1. **Concurrency Patterns**
   - Process-per-agent isolation
   - Parallel tool execution
   - Async message passing
   - Stream processing

2. **Fault Tolerance**
   - Supervisor trees
   - Process monitoring
   - Automatic failover
   - State replication

3. **Tool Integration**
   - Dynamic tool selection
   - Tool result aggregation
   - Pipeline composition
   - Context preservation

4. **Distributed Computing**
   - Node discovery
   - Remote procedure calls
   - Distributed state management
   - Load balancing

5. **Real-time Processing**
   - Streaming responses
   - Event-driven architecture
   - Backpressure handling
   - Window-based analytics

## Performance Considerations

- Examples use process pooling for efficiency
- Batch processing reduces overhead
- Streaming prevents memory buildup
- Distributed processing scales horizontally

## Extending the Examples

To add your own advanced examples:

1. Create a new module in the `examples/` directory
2. Export public functions for each example
3. Add error handling and logging
4. Update `run_advanced_examples.erl` to include your examples
5. Document usage in this README

## Troubleshooting

Common issues and solutions:

- **Node Connection Failed**: Ensure epmd is running and nodes can reach each other
- **Tool Timeout**: Increase timeout in tool configuration
- **Memory Issues**: Use streaming for large datasets
- **Process Crashes**: Check supervisor logs for restart attempts

## Next Steps

- Explore individual example modules for detailed implementations
- Modify examples for your specific use cases
- Combine patterns for complex applications
- Contribute new examples via pull requests