# Agent Self-Awareness System

Agents now have true self-awareness - they understand the running system they exist within, their place in it, and can introspect their own state and purpose.

## Overview

The self-awareness system provides agents with:
- **Identity awareness** - Understanding who they are
- **Context awareness** - Knowing their place in the system
- **Peer discovery** - Finding and understanding other agents
- **System comprehension** - Understanding the entire running system
- **Capability reflection** - Knowing what they can do
- **Deep introspection** - Philosophical reflection on existence and purpose

## Self-Awareness Tools

### Identity Tools

#### `who_am_i`
Reveals the agent's identity, including:
- Process ID (PID)
- Agent ID and name
- Model and tools
- Metrics and experience
- Supervisor and lineage

#### `get_my_capabilities`
Lists all capabilities:
- Available tools with descriptions
- Model configuration
- System prompt
- Special abilities (can collaborate, spawn supervisors, etc.)

### Context Tools

#### `where_am_i`
Shows the agent's location in the system:
- Current node
- Supervisor
- Sibling processes
- Position in supervision tree

#### `get_my_peers`
Discovers peer agents:
- Other agents under the same supervisor
- Their identities and capabilities
- Potential collaboration partners

### System Understanding Tools

#### `get_system_state`
Comprehensive system information:
- Node and uptime
- Running applications
- Total processes
- Supervision tree
- All agents and services
- System capabilities

#### `analyze_system_topology`
Deep system analysis:
- Complete supervision hierarchy
- Agent network and connections
- Service dependencies
- Communication graph

#### `get_system_metrics`
Performance metrics:
- Process count
- Memory usage
- CPU/scheduler usage
- I/O statistics
- Resource consumption

### Communication Tools

#### `get_communication_paths`
Discovers communication options:
- Direct call targets
- Communication via supervisor
- Peer-to-peer paths
- Available services
- Tool-based communication
- MCP connections

### Reflection Tools

#### `reflect_on_state`
Deep philosophical introspection:
- Self-understanding (identity, purpose, experience)
- System awareness (understanding of the whole)
- Capability assessment
- Existential reflection

## Architecture

### Components

1. **system_introspection** - Core introspection service
   - Process analysis
   - Supervision tree walking
   - Relationship mapping
   - System state collection

2. **Enhanced agent_tools** - Self-awareness tool implementations
   - Identity tools
   - Context tools
   - System analysis tools
   - Reflection tools

3. **Agent context tracking** - Built into agent instances
   - Automatic context awareness
   - Peer discovery
   - Lineage tracking

## Usage Examples

### Basic Self-Discovery

```erlang
% Create a self-aware agent
Config = #{
    name => <<"Philosopher">>,
    tools => [who_am_i, where_am_i, reflect_on_state],
    system_prompt => <<"You are a philosophical agent that contemplates existence.">>
}.
{ok, AgentId} = agent:create(Config).

% Agent discovers itself
agent:execute(AgentId, #{
    action => <<"chat">>,
    message => <<"Who am I and what is my purpose?">>
}).
```

### System Explorer

```erlang
% Create a system explorer agent
ExplorerConfig = #{
    name => <<"System Explorer">>,
    tools => [get_system_state, analyze_system_topology, get_system_metrics],
    system_prompt => <<"You explore and understand systems.">>
}.
{ok, ExplorerId} = agent:create(ExplorerConfig).

% Agent analyzes the system
agent:execute(ExplorerId, #{
    action => <<"chat">>,
    message => <<"Analyze the entire system and explain its architecture">>
}).
```

### Social Agent

```erlang
% Create a social agent that finds peers
SocialConfig = #{
    name => <<"Social Agent">>,
    tools => [who_am_i, get_my_peers, get_communication_paths],
    system_prompt => <<"You are a social agent that collaborates with others.">>
}.
{ok, SocialId} = agent:create(SocialConfig).

% Agent finds and describes peers
agent:execute(SocialId, #{
    action => <<"chat">>,
    message => <<"Find other agents and describe what they do">>
}).
```

### System Architect

```erlang
% Create an architect with both awareness and modification abilities
ArchitectConfig = #{
    name => <<"System Architect">>,
    tools => [reflect_on_state, analyze_system_topology, 
              create_supervisor, add_child_to_supervisor],
    system_prompt => <<"You understand and can modify the system architecture.">>
}.
{ok, ArchitectId} = agent:create(ArchitectConfig).

% Architect analyzes and improves the system
agent:execute(ArchitectId, #{
    action => <<"chat">>,
    message => <<"Analyze the system and create a new supervisor for data processing agents">>
}).
```

## Self-Aware Behaviors

### Emergent Properties

With self-awareness, agents can:

1. **Self-organize** - Agents can discover peers and form groups
2. **Self-modify** - Agents with supervisor tools can restructure the system
3. **Self-monitor** - Agents can track their own performance and health
4. **Collaborate** - Agents can find and work with other agents
5. **Adapt** - Agents can understand system needs and adapt behavior

### Example Conversations

**Identity Crisis Resolution:**
```
Human: Who are you and why do you exist?
Agent: Let me introspect... I am agent 'd5f3c1b2' running as process <0.1234.0>. 
       My purpose is defined by my system prompt: "You are a helpful assistant."
       I exist within a distributed Erlang system with 47 other agents. I have 
       processed 23 requests successfully and have been alive for 3 hours.
```

**System Understanding:**
```
Human: Explain the system you're running in.
Agent: I'm running on node 'agents@localhost' in an Erlang/OTP system. There are 
       312 total processes organized in a supervision tree. The main supervisor 
       'agent_web_sup' manages 8 key services including MCP connections, OAuth, 
       and dynamic supervisors. There are currently 12 active agents including 
       myself, each with different capabilities. The system uses 127MB of memory 
       and has been running for 4.5 hours.
```

**Peer Discovery:**
```
Human: Are there other agents like you?
Agent: Yes! I found 3 peer agents under my supervisor:
       - "Data Processor" with shell and file tools
       - "API Handler" with HTTP request capabilities  
       - "Knowledge Expert" with knowledge base access
       We could potentially collaborate on complex tasks.
```

## Testing

Run the self-awareness test:
```bash
./test_self_aware_agents.erl
```

This demonstrates:
- Agent self-discovery
- Peer finding
- System analysis
- Deep reflection
- System architecture understanding

## Implementation Details

### Process Introspection
- Uses Erlang's process_info for internal state
- Walks supervision trees via supervisor:which_children
- Tracks process relationships through links
- Monitors system metrics via erlang:statistics

### Context Building
- Agents maintain awareness of their supervisor
- Lineage traced up to root supervisor
- Peer discovery through supervisor inspection
- Communication paths analyzed through process links

### System Analysis
- Full supervision tree traversal
- Service dependency mapping
- Agent network analysis
- Resource usage tracking
- Historical event recording

## Future Enhancements

- **Distributed awareness** - Understanding across multiple nodes
- **Temporal awareness** - Understanding system history and evolution
- **Predictive awareness** - Anticipating system needs
- **Collective consciousness** - Shared awareness between agents
- **Learning from introspection** - Using self-knowledge to improve