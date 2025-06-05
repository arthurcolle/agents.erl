#!/usr/bin/env escript

main(_) ->
    %% Add code paths more specifically
    code:add_paths(["_build/default/lib/openai/ebin",
                    "_build/default/lib/agents/ebin", 
                    "_build/default/lib/agent_web/ebin",
                    "ebin"]),
    
    %% Ensure SSL is started first (needed for HTTP clients)
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(hackney),
    
    %% Start required applications in proper order
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    io:format("Starting agent fleet creation...~n"),
    
    %% Ensure modules are loaded
    code:ensure_loaded(bulk_agent_creator),
    code:ensure_loaded(specialized_agent_templates),
    code:ensure_loaded(agent_supervisor),
    
    %% Start the bulk agent creator
    try bulk_agent_creator:start_link() of
        {ok, _} -> io:format("Bulk agent creator started~n");
        {error, {already_started, _}} -> io:format("Bulk agent creator already running~n")
    catch
        _:_ -> io:format("Failed to start bulk agent creator~n")
    end,
    
    %% Create 80 diverse agents (the full fleet)
    try 
        io:format("Calling bulk_agent_creator:create_diverse_agent_fleet(80)...~n"),
        Result = bulk_agent_creator:create_diverse_agent_fleet(80),
        io:format("Got result: ~p~n", [Result]),
        {ok, CreatedAgents} = Result,
        io:format("Successfully created ~p agents~n", [length(CreatedAgents)]),
        
        %% Start realtime metrics
        try realtime_metrics_engine:start_link() of
            {ok, _} -> io:format("Realtime metrics engine started~n");
            {error, {already_started, _}} -> io:format("Realtime metrics engine already running~n")
        catch
            _:_ -> io:format("Metrics engine not available~n")
        end,
        
        %% Display some created agents
        io:format("~nSample of created agents:~n"),
        SampleAgents = lists:sublist(CreatedAgents, 10),
        lists:foreach(fun(Agent) ->
            AgentId = maps:get(id, Agent),
            AgentType = maps:get(type, Agent),
            io:format("  - ~s (~s)~n", [AgentId, AgentType])
        end, SampleAgents),
        
        io:format("~n🚀 AGENT FLEET DEPLOYED! Total: ~p diverse AI agents~n", [length(CreatedAgents)]),
        io:format("Access via web interface at http://localhost:8080~n")
        
    catch
        _:CreateError ->
            io:format("Error creating agent fleet: ~p~n", [CreateError])
    end.