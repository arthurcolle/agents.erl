-module(demo_agent_scheduling).
-export([run/0]).

%% Demo of the advanced agent scheduling capabilities

run() ->
    io:format("~n=== Agent Scheduling & Temporal Awareness Demo ===~n~n"),
    
    %% Ensure the system is running
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    timer:sleep(1000),
    
    %% Create a test agent
    AgentConfig = #{
        name => <<"Scheduler Agent">>,
        type => assistant,
        model => <<"gpt-4o-mini">>,
        system_prompt => <<"You are a helpful scheduling assistant that can plan and execute tasks in the future.">>,
        tools => [read_file, search_documentation, execute_code]
    },
    
    {ok, AgentId} = agent_supervisor:start_agent(AgentConfig),
    io:format("Created agent: ~p~n", [AgentId]),
    
    %% Enable temporal awareness for the agent
    ok = agent_temporal_awareness:enable_temporal_awareness(AgentId),
    io:format("Enabled temporal awareness for agent~n"),
    
    %% Schedule some tasks for the future
    demo_schedule_tasks(AgentId),
    
    %% Show agent's temporal context
    demo_temporal_context(AgentId),
    
    %% Demonstrate timeline event retrieval
    demo_timeline_events(),
    
    %% Show upcoming tasks
    demo_upcoming_tasks(),
    
    %% Demonstrate real-time event streaming
    demo_event_streaming(),
    
    io:format("~n=== Demo Complete ===~n").

demo_schedule_tasks(AgentId) ->
    io:format("~n--- Scheduling Tasks ---~n"),
    
    %% Schedule a task for 5 seconds from now
    Task1 = #{
        action => <<"analyze">>,
        message => <<"Analyze the current system performance">>,
        tools => [system_metrics]
    },
    {ok, TaskId1} = agent_instance:schedule_task(AgentId, Task1, 5),
    io:format("Scheduled task 1 (in 5 seconds): ~p~n", [TaskId1]),
    
    %% Schedule a task for 10 seconds from now
    Task2 = #{
        action => <<"generate">>,
        message => <<"Generate a daily report of agent activities">>,
        context => #{report_type => daily}
    },
    {ok, TaskId2} = agent_instance:schedule_task(AgentId, Task2, 10),
    io:format("Scheduled task 2 (in 10 seconds): ~p~n", [TaskId2]),
    
    %% Schedule a recurring task (every 30 seconds)
    Task3 = #{
        action => <<"monitor">>,
        message => <<"Check system health and alert if issues">>,
        recurring => true,
        interval => 30
    },
    {ok, TaskId3} = agent_instance:schedule_task(AgentId, Task3, 30),
    io:format("Scheduled recurring task 3 (every 30 seconds): ~p~n", [TaskId3]),
    
    %% Get all scheduled tasks for the agent
    {ok, ScheduledTasks} = agent_instance:get_scheduled_tasks(AgentId),
    io:format("Total scheduled tasks: ~p~n", [length(ScheduledTasks)]).

demo_temporal_context(AgentId) ->
    io:format("~n--- Temporal Context ---~n"),
    
    %% Get the agent's temporal context
    {ok, Context} = agent_temporal_awareness:get_temporal_context(AgentId),
    
    io:format("Temporal Summary:~n"),
    Summary = maps:get(temporal_summary, Context, #{}),
    io:format("  - Total events: ~p~n", [maps:get(total_events, Summary, 0)]),
    io:format("  - Pattern count: ~p~n", [maps:get(pattern_count, Summary, 0)]),
    io:format("  - Temporal state: ~p~n", [maps:get(temporal_state, Summary, idle)]),
    io:format("  - Recent event types: ~p~n", [maps:get(recent_event_types, Summary, [])]),
    
    %% Analyze temporal patterns
    {ok, Patterns} = agent_temporal_awareness:analyze_temporal_patterns(AgentId),
    io:format("~nTemporal Patterns:~n"),
    io:format("  - Recurring events: ~p~n", [maps:get(recurring_events, Patterns, [])]),
    io:format("  - Event sequences: ~p~n", [maps:size(maps:get(sequences, Patterns, #{}))]).

demo_timeline_events() ->
    io:format("~n--- Timeline Events ---~n"),
    
    %% Retrieve recent timeline events
    {ok, Events} = timeline_event_store:retrieve(10, 0),
    io:format("Recent events (~p total):~n", [length(Events)]),
    
    lists:foreach(fun(Event) ->
        Type = maps:get(type, Event, unknown),
        AgentId = maps:get(agent_id, Event, undefined),
        io:format("  - ~p (agent: ~p)~n", [Type, AgentId])
    end, lists:sublist(Events, 5)),
    
    %% Get total event count
    EventCount = timeline_event_store:get_event_count(),
    io:format("Total events in timeline: ~p~n", [EventCount]).

demo_upcoming_tasks() ->
    io:format("~n--- Upcoming Tasks ---~n"),
    
    %% Get tasks scheduled for the next 60 seconds
    {ok, UpcomingTasks} = agent_scheduler_engine:get_upcoming_tasks(60),
    
    io:format("Tasks in next 60 seconds: ~p~n", [length(UpcomingTasks)]),
    lists:foreach(fun(Task) ->
        TaskId = element(2, Task),  % Extract task ID from record
        Status = element(5, Task),  % Extract status
        io:format("  - Task ~p (status: ~p)~n", [TaskId, Status])
    end, UpcomingTasks).

demo_event_streaming() ->
    io:format("~n--- Event Streaming Setup ---~n"),
    
    %% Set up a simple event stream listener
    Self = self(),
    FilterFun = fun(_Event) -> true end,  % Accept all events
    
    ok = timeline_event_store:stream_events(FilterFun, Self),
    io:format("Streaming listener registered~n"),
    
    %% Create a test event to trigger streaming
    timeline_event_store:store(#{
        type => demo_event,
        message => <<"This is a demo event">>,
        timestamp => erlang:timestamp()
    }),
    
    %% Check for streamed events (non-blocking)
    receive
        {timeline_event, Event} ->
            io:format("Received streamed event: ~p~n", [maps:get(type, Event, unknown)])
    after 100 ->
        io:format("No events received in stream (expected in async operation)~n")
    end,
    
    %% Show WebSocket endpoint for real-time streaming
    io:format("~nWebSocket endpoint available at: ws://localhost:8080/ws/timeline~n"),
    io:format("Connect to receive real-time timeline events and scheduling updates~n").