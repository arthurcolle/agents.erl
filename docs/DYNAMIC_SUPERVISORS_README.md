# Dynamic Supervisor Management

This system now supports dynamic supervisor creation and runtime supervision tree augmentation. Agents can spawn new supervisors and modify the system structure without restarts.

## Features

### 1. Dynamic Supervisor Creation
- Create supervisors at runtime with configurable restart strategies
- Support for all OTP supervisor strategies (one_for_one, one_for_all, rest_for_one, simple_one_for_one)
- Hierarchical supervisor structures

### 2. Runtime System Augmentation
- Add new services and components without restarting the system
- Spawn agents under specific supervisors
- Create agent groups with dedicated supervisors
- Hot code loading support

### 3. Agent-Accessible Tools
Agents can now use these tools to manage the system:
- `create_supervisor` - Create new supervisors
- `stop_supervisor` - Stop and remove supervisors
- `list_supervisors` - List all active supervisors
- `add_child_to_supervisor` - Add processes to supervisors
- `get_supervision_tree` - View the complete supervision tree

## Usage

### In the Rebar Shell

```erlang
% Start the system
rebar3 shell

% Create a new supervisor
runtime_system:create_sup(my_sup, one_for_one).

% Spawn an agent under the supervisor
AgentConfig = #{
    name => <<"My Agent">>,
    model => <<"gpt-4">>,
    tools => [shell, file_read]
}.
runtime_system:spawn_agent_under(my_sup, my_agent, AgentConfig).

% Create an agent group
runtime_system:spawn_agent_group(worker_pool, 5, #{
    model => <<"gpt-3.5-turbo">>,
    tools => [shell]
}).

% View the supervision tree
runtime_system:tree().

% Check system health
runtime_system:health().

% Augment with a new service
runtime_system:augment(my_service, my_module).
```

### Through Agents

Agents can manage supervisors using their tools:

```erlang
% Create an agent with supervisor management capabilities
Config = #{
    name => <<"System Manager">>,
    tools => [create_supervisor, list_supervisors, add_child_to_supervisor]
}.
{ok, AgentId} = agent:create(Config).

% Ask the agent to create a supervisor
agent:execute(AgentId, #{
    action => <<"chat">>,
    message => <<"Create a new supervisor called data_processors with one_for_one strategy">>
}).
```

## Architecture

### Components

1. **dynamic_supervisor_manager** - Gen_server that manages dynamic supervisors
2. **dynamic_supervisor_wrapper** - Supervisor behavior implementation
3. **runtime_system** - Shell convenience functions
4. **agent_tools** - Extended with supervisor management tools

### Supervision Tree Structure

```
agent_web_sup
├─ dynamic_supervisor_manager
├─ [dynamic supervisors]
│  ├─ supervisor_1
│  │  └─ [children]
│  └─ supervisor_2
│     └─ [children]
└─ [other services]
```

## Examples

### Creating a Multi-Tier Architecture

```erlang
% Create top-level supervisors
runtime_system:create_sup(frontend_sup, one_for_one).
runtime_system:create_sup(backend_sup, one_for_all).
runtime_system:create_sup(data_sup, rest_for_one).

% Add services to each tier
runtime_system:spawn_agent_under(frontend_sup, web_agent, #{
    name => <<"Web Interface Agent">>,
    tools => [http_request]
}).

runtime_system:spawn_agent_under(backend_sup, api_agent, #{
    name => <<"API Agent">>,
    tools => [shell, file_read, file_write]
}).

runtime_system:spawn_agent_under(data_sup, db_agent, #{
    name => <<"Database Agent">>,
    tools => [knowledge_base_retrieval]
}).
```

### Self-Modifying System

```erlang
% Create an agent that can modify the system
SysAgent = #{
    name => <<"System Architect">>,
    tools => [create_supervisor, add_child_to_supervisor, get_supervision_tree],
    system_prompt => <<"You manage the system architecture. Create supervisors and spawn agents as needed.">>
}.
{ok, ArchitectId} = agent:create(SysAgent).

% The agent can now respond to requests like:
% "Create a load-balanced worker pool with 10 agents"
% "Add a caching layer to the system"
% "Spawn a monitoring agent under the main supervisor"
```

## Testing

Run the test script to see all features in action:

```bash
./test_dynamic_supervisors.erl
```

Or compile and run the test:

```bash
rebar3 compile
escript test_dynamic_supervisors.erl
```

## Safety Features

- Supervisors are isolated - crashes in dynamic supervisors don't affect the main system
- All operations are validated before execution
- Circular dependencies are prevented
- Resource limits can be configured
- Failed supervisors are automatically cleaned up

## Future Enhancements

- Supervisor templates for common patterns
- Automatic load balancing
- Distributed supervisor support
- Persistence of supervisor configurations
- GUI for supervision tree visualization