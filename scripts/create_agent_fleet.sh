#!/bin/bash

# Agent Fleet Creation Script
# Creates 50-100 diverse AI agents with multi-turn function calling

echo "Creating Agent Fleet with diverse AI agents..."

cd "$(dirname "$0")/.."

# Start the Erlang shell and execute the bulk agent creation
./rebar3 shell --eval "
    %% Start required applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web),
    
    %% Create the diverse agent fleet
    {ok, CreatedAgents} = bulk_agent_creator:create_agent_fleet(80),
    
    %% Start the realtime metrics engine
    realtime_metrics_engine:start_link(),
    
    %% Print results
    io:format(\"Successfully created ~p agents~n\", [length(CreatedAgents)]),
    lists:foreach(fun(Agent) ->
        AgentId = maps:get(id, Agent),
        AgentType = maps:get(type, Agent),
        io:format(\"Created agent: ~s (~s)~n\", [AgentId, AgentType])
    end, CreatedAgents),
    
    io:format(\"~nAgent fleet is ready! Access via web interface at http://localhost:8080~n\"),
    io:format(\"Real-time metrics and Q-table learning are active.~n\")
." --apps eunit,inets,ssl,jsx