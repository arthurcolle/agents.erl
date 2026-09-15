# Runtime Architecture

This document describes the process ownership model implemented by the
current code. It is the canonical architecture reference for the runtime.

## Application boundaries

The umbrella has a one-way dependency graph:

```text
openai
  ↑
agents
  ↑
agent_web
```

Each OTP application callback starts exactly one root supervisor. Dependency
startup belongs to the OTP application controller through each `.app` file.

```text
openai_app
└── openai_sup

agents_app
└── agents_sup
    ├── platform registries and services
    ├── agent_supervisor
    │   └── dynamic agent_instance children
    ├── super_agent_sup
    │   └── optional super_agent controller
    ├── dynamic_runtime_sup (rest_for_one)
    │   ├── dynamic_supervisors_sup
    │   │   └── runtime-created supervisors
    │   └── dynamic_supervisor_manager
    └── meta_system_sup

agent_web_app
└── agent_web_sup (rest_for_one)
    ├── agent_web_services_sup
    │   ├── agent_web_storage_sup (rest_for_one)
    │   │   ├── persistent_table_manager
    │   │   └── conversation_initializer
    │   └── agent_web_observability_sup
    ├── Cowboy listener
    └── agent_web_bootstrap
```

## Ownership rules

- `agent.erl` is a public API module, not an application callback or
  supervisor.
- `agents_sup` owns long-lived platform services.
- `agent_supervisor` owns agent instances only. Agent creation and termination
  must go through its lifecycle API.
- `super_agent_sup` owns the optional singleton super-agent controller; its
  underlying `agent_instance` still belongs to `agent_supervisor`.
- `dynamic_supervisors_sup` owns runtime-created supervisors. The manager
  records and operates on them but never attaches core children to
  `agent_web_sup`.
- HTTP handlers and workflow processes never `start_link` an agent directly.
- `agent_registry` is an index of supervised agents, not a second lifecycle
  owner. Registration and removal are synchronous.
- Monitoring processes observe supervisor failures. OTP supervisors perform
  recovery; observers do not manually restart their ancestors.
- Post-start initialization and periodic health checks run in the supervised
  `agent_web_bootstrap` worker.

## Startup ordering

The web root uses `rest_for_one` so services are available before Cowboy
accepts traffic and the bootstrap worker runs last. Storage uses its own
`rest_for_one` subtree because conversation initialization depends on the
persistent table manager.

For tests and constrained deployments, bootstrap behavior can be configured:

```erlang
{agent_web, [
    {bootstrap_enabled, true},
    {bootstrap_delay_ms, 1000},
    {health_monitor_enabled, true},
    {health_monitor_interval_ms, 30000}
]}.
```

The knowledge-base path is configured with the `agents` application
environment key `knowledge_base_dir`; it defaults to `knowledge_bases`
relative to the runtime working directory.

Timeline persistence is configured with `timeline_event_path`. Startup replay
restores records directly into ETS and never invokes the append path.

## Lifecycle contract

`agent_supervisor:start_agent/1,2` assigns a stable child ID, starts a
`transient` child, and records it in `agent_registry`. Abnormal exits are
restarted by OTP. Explicit termination removes both the child specification
and registry entry.

Use:

```erlang
{ok, Pid} = agent_supervisor:start_agent(AgentId, Config),
{ok, Agents} = agent_supervisor:get_all_agents(),
ok = agent_supervisor:terminate_agent(AgentId).
```
