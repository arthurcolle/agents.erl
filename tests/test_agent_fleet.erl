#!/usr/bin/env escript

main(_) ->
    %% Add code paths
    code:add_paths(["_build/default/lib/*/ebin", "ebin"]),
    
    %% Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    %% Start the realtime metrics engine
    try realtime_metrics_engine:start_link() of
        {ok, _} -> io:format("Realtime metrics engine started~n");
        {error, {already_started, _}} -> io:format("Realtime metrics engine already running~n")
    catch
        _:MetricsError -> io:format("Error starting metrics engine: ~p~n", [MetricsError])
    end,
    
    %% Create a test fleet (10 agents)
    try bulk_agent_creator:create_agent_fleet(10) of
        {ok, CreatedAgents} ->
            io:format("Successfully created ~p agents~n", [length(CreatedAgents)]),
            lists:foreach(fun(Agent) ->
                AgentId = maps:get(id, Agent),
                AgentType = maps:get(type, Agent),
                io:format("Created agent: ~s (~s)~n", [AgentId, AgentType])
            end, CreatedAgents),
            
            io:format("~nAgent fleet test completed successfully!~n")
    catch
        _:CreateError ->
            io:format("Error creating agent fleet: ~p~n", [CreateError])
    end.