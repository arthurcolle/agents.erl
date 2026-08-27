# agents.erl

**An Erlang/OTP operating system for autonomous agents.**
Supervision trees instead of orchestrators. Mnesia instead of a database. Message
passing instead of HTTP glue. ~1,150 Erlang sources across two OTP applications — the
distributed-systems research arm that gives Distributed Systems, Inc. its name.

---

## Why Erlang

Autonomous agents are long-lived, failure-prone, stateful processes that must be
individually supervised, restarted, hot-upgraded, and coordinated across a cluster.
That is *exactly* the problem OTP was built for in 1986. agents.erl applies five decades
of telecom-grade fault tolerance to the agent problem — while the production
microservices path lives in `dsco-autobot` (Python/FastAPI), this codebase proves the
"autonomous distributed systems agent" thesis at the runtime level.

## Applications

| App | Modules | What it is |
|---|---|---|
| `apps/agents` | 136 | Core agent runtime: supervisors, registries, schedulers, engines |
| `apps/agent_web` | 173 | Cowboy REST + WebSocket surface, fleet management, observability, MCP, Next.js frontend |
| `apps/openai` | 28 | OpenAI-compatible API surface |
| `apps/openapi_scaffold` | 24 | OpenAPI 3.0 scaffolding + JSON-Schema validation (jesse) |

**Core runtime** (`apps/agents/src`):

- `agent_supervisor`, `dynamic_runtime_sup`, `evolutionary_supervision` — OTP
  supervision incl. *adaptive* supervision strategies
- `super_agent` / `agent_instance` — the agent process abstraction with tool use
- `agent_registry`, `agent_discovery`, `dynamic_agent_router` — cluster-wide agent
  addressing and routing
- `agent_quorum` + `distributed_consensus_engine` — quorum decisions across nodes
- `swarm_intelligence_engine` — emergent multi-agent coordination
- `riak_dt_map` — CRDT-based convergent agent state (convergent replicated data types)
- `agent_scheduler`, `agent_time_authority` — temporal scheduling with cluster time
  authority

**Agent cognition engines** (selection):

`meta_cognitive_awareness_engine`, `neural_symbolic_reasoning_engine`,
`backtracking_engine` / `backward_chaining_engine`, `environmental_learning_engine`,
`self_modifying_agent` + `self_modifying_agent_engine`, `deep_code_reflection_engine`,
`autonomous_knowledge_ingestion`, `hot_swappable_ai_models`,
`model_selection_strategy`, `system_introspection`.

**Operational hardening** (`apps/agent_web/src`):

- `auto_healing_coordinator`, `autonomous_error_corrector`, `crash_ai_analyzer`,
  `ai_error_processor` — agents that fix their own failures
- `fleet_management` + `bulk_agent_creation` — operate hundreds of agents at once
- `cost_tracking_handler`, `api_key_manager`, JWT auth, Prometheus metrics
  (`prometheus_httpd`), OpenAPI scaffold with JSON-Schema validation (jesse)
- `simple_mcp_handler` — Model Context Protocol integration

## Real-time scheduling & temporal awareness

`REALTIME_AGENT_SCHEDULING.md` — three subsystems:

1. **`agent_scheduler_engine`** — schedule tasks at absolute or relative times
   (`"+1h"`), retry with backoff, reschedule, query time windows
2. **`timeline_event_store`** — persistent event timeline with real-time WebSocket
   streaming and retention management
3. **`agent_temporal_awareness`** — agents reason over past events, detect recurring
   patterns, and *predict future events*

## Distribution

```bash
./start_cluster.sh       # multi-node cluster
./start_distributed.sh   # distributed Erlang mode
./hot_swap.sh            # live code upgrade without downtime
./verify_system.sh       # end-to-end system check
```

State persists in **Mnesia** across nodes (see `Mnesia.agents@chrysalis` — the cluster
has run multi-node).

## Build & test

```bash
./build_release.sh       # rebar3 release build
./run_all_tests.sh       # full test suite
```

40+ test modules cover fleet management, autonomous multi-turn function calling,
streaming tool calls, cost tracking, self-scaffolding, dynamic supervisors, and error
self-repair.

## Dependencies

cowboy (HTTP/WS), gun + hackney (HTTP clients), jiffy/jsx (JSON), gproc (process
registry), Prometheus metrics, JWT, jesse (OpenAPI validation), yamerl.

---

*Part of the Distributed Systems, Inc. platform: `dsco-cli` (runtime, C) ·
`dsco-autobot` (production microservices, Python) · `agents.erl` (distributed runtime,
Erlang/OTP) · GraphSub (graph substrate, Rust).*
