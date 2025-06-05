# Real-Time Agent Scheduling System

## Overview

The agents.erl system now includes advanced scheduling capabilities that allow agents to:
- Schedule tasks for future execution
- Maintain temporal awareness of past, present, and future events
- Stream events in real-time via WebSockets
- Analyze temporal patterns and predict future events

## Key Components

### 1. Agent Scheduler Engine (`agent_scheduler_engine`)
Manages scheduled tasks for all agents in the system.

**Features:**
- Schedule tasks for specific times or relative times (e.g., "+1h", "+30m")
- Cancel, reschedule, or execute tasks immediately
- Automatic retry on failure with configurable max retries
- Query upcoming tasks within a time window

### 2. Timeline Event Store (`timeline_event_store`)
Persistent storage for all system events with real-time streaming.

**Features:**
- Store events with automatic timestamping
- Retrieve events by agent, type, or time range
- Real-time event streaming to connected clients
- Automatic cleanup of old events (configurable retention)

### 3. Agent Temporal Awareness (`agent_temporal_awareness`)
Provides agents with understanding of their temporal context.

**Features:**
- Track past events and current state
- Analyze temporal patterns (recurring events, sequences)
- Predict future events based on patterns
- Configurable temporal awareness window

## API Usage

### Scheduling Tasks

```erlang
%% Schedule a task for 5 seconds from now
Task = #{
    action => <<"analyze">>,
    message => <<"Analyze system performance">>,
    tools => [system_metrics]
},
{ok, TaskId} = agent_instance:schedule_task(AgentId, Task, 5).

%% Schedule for specific time
{ok, TaskId} = agent_instance:schedule_task(AgentId, Task, {1735, 846800, 0}).

%% Get scheduled tasks
{ok, Tasks} = agent_instance:get_scheduled_tasks(AgentId).
```

### Temporal Awareness

```erlang
%% Enable temporal awareness
ok = agent_temporal_awareness:enable_temporal_awareness(AgentId).

%% Get temporal context
{ok, Context} = agent_temporal_awareness:get_temporal_context(AgentId).

%% Analyze patterns
{ok, Patterns} = agent_temporal_awareness:analyze_temporal_patterns(AgentId).
```

### Timeline Events

```erlang
%% Store an event
timeline_event_store:store(#{
    type => task_completed,
    agent_id => AgentId,
    result => Result
}).

%% Retrieve recent events
{ok, Events} = timeline_event_store:retrieve(50, 0).

%% Retrieve by agent
{ok, AgentEvents} = timeline_event_store:retrieve_by_agent(AgentId, #{limit => 20}).
```

## REST API Endpoints

### Scheduling
- `POST /api/scheduling/tasks` - Schedule a new task
- `GET /api/scheduling/tasks` - List all scheduled tasks
- `GET /api/scheduling/agents/{agent_id}/tasks` - Get tasks for specific agent
- `DELETE /api/scheduling/tasks/{task_id}` - Cancel a task
- `PUT /api/scheduling/tasks/{task_id}` - Reschedule a task
- `POST /api/scheduling/tasks/{task_id}/execute` - Execute immediately
- `GET /api/scheduling/upcoming?window=3600` - Get upcoming tasks

### Temporal Context
- `GET /api/scheduling/agents/{agent_id}/temporal` - Get temporal context
- `POST /api/scheduling/agents/{agent_id}/temporal/enable` - Enable awareness

## WebSocket Streaming

Connect to `ws://localhost:8080/ws/timeline` for real-time events.

### Message Types

**Subscribe to events:**
```json
{
  "type": "subscribe",
  "topic": "timeline_events",
  "options": {
    "event_types": ["task_scheduled", "task_completed"],
    "agent_ids": ["agent_123"]
  }
}
```

**Receive events:**
```json
{
  "type": "timeline_event",
  "event": {
    "id": "event_123",
    "type": "task_completed",
    "agent_id": "agent_123",
    "timestamp": [1735, 846800, 0],
    "data": {...}
  }
}
```

## Example: Creating a Self-Scheduling Agent

```erlang
%% Create an agent that schedules its own tasks
AgentConfig = #{
    name => <<"Self-Scheduler">>,
    type => assistant,
    system_prompt => <<"You can schedule tasks for yourself in the future.">>,
    tools => [schedule_task, get_scheduled_tasks]
},
{ok, AgentId} = agent_supervisor:start_agent(AgentConfig),

%% Enable temporal awareness
agent_temporal_awareness:enable_temporal_awareness(AgentId),

%% The agent can now schedule tasks for itself
agent_instance:execute(AgentId, #{
    action => <<"chat">>,
    message => <<"Schedule a task to check the weather in 1 hour">>
}).
```

## Architecture Benefits

1. **Real-time Responsiveness**: WebSocket streaming provides instant updates
2. **Temporal Intelligence**: Agents can learn from patterns and predict future needs
3. **Reliability**: Automatic retries and failure handling for scheduled tasks
4. **Scalability**: Efficient event storage with automatic cleanup
5. **Flexibility**: Support for one-time and recurring tasks

This scheduling system transforms agents from reactive to proactive, enabling them to plan and execute complex workflows over time.