%%%-------------------------------------------------------------------
%%% @doc Self-Scaffolding Demo
%%% Demonstrates the self-scaffolding system with hourly updates
%%% @end
%%%-------------------------------------------------------------------
-module(self_scaffold_demo).

-export([run/0]).

run() ->
    io:format("=== Self-Scaffolding System Demo ===~n~n"),
    
    % Start the web application if not running
    ensure_app_started(),
    
    % Demonstrate the self-scaffolding features
    demo_endpoint_discovery(),
    demo_schedule_info(),
    demo_force_update(),
    demo_endpoint_registry(),
    
    io:format("~n=== Demo Complete ===~n").

ensure_app_started() ->
    case application:ensure_all_started(agent_web) of
        {ok, _} ->
            io:format("Agent web application started~n");
        {error, {already_started, _}} ->
            io:format("Agent web application already running~n");
        {error, Reason} ->
            io:format("Failed to start agent web: ~p~n", [Reason])
    end.

demo_endpoint_discovery() ->
    io:format("~n1. Endpoint Discovery Status:~n"),
    
    Status = endpoint_discovery:get_discovery_status(),
    io:format("  Current status: ~p~n", [maps:get(status, Status)]),
    io:format("  Last discovery: ~p~n", [maps:get(last_discovery, Status)]),
    io:format("  Discovery interval: ~p ms (~p hours)~n", 
             [maps:get(discovery_interval, Status), 
              maps:get(discovery_interval, Status) / 3600000]),
    
    % Trigger a discovery for OpenAI
    io:format("~n  Discovering OpenAI endpoints...~n"),
    case endpoint_discovery:discover_endpoints(openai) of
        {ok, Count} ->
            io:format("  Discovered ~p endpoints from OpenAI~n", [Count]);
        {error, Reason} ->
            io:format("  Discovery failed: ~p~n", [Reason])
    end.

demo_schedule_info() ->
    io:format("~n2. Scheduler Information:~n"),
    
    Info = scaffold_scheduler:get_schedule_info(),
    io:format("  Active schedules:~n"),
    maps:foreach(fun(Name, Schedule) ->
        io:format("    - ~p: runs every ~p ms, last run: ~p, count: ~p~n",
                 [Name, 
                  maps:get(interval, Schedule),
                  maps:get(last_run, Schedule),
                  maps:get(run_count, Schedule)])
    end, maps:get(schedules, Info)),
    
    io:format("  Total updates: ~p~n", [maps:get(total_updates, Info)]),
    io:format("  Last update: ~p~n", [maps:get(last_update, Info)]).

demo_force_update() ->
    io:format("~n3. Forcing Update Now:~n"),
    
    ok = scaffold_scheduler:force_update_now(),
    io:format("  Update triggered - check logs for results~n"),
    
    % Wait a bit for updates to complete
    timer:sleep(2000).

demo_endpoint_registry() ->
    io:format("~n4. Registered Endpoints:~n"),
    
    Endpoints = endpoint_registry:get_all_endpoints(),
    
    % Group by tag
    GroupedEndpoints = lists:foldl(fun(Endpoint, Acc) ->
        Tag = element(6, Endpoint), % #endpoint.tag
        Current = maps:get(Tag, Acc, []),
        maps:put(Tag, [Endpoint | Current], Acc)
    end, #{}, Endpoints),
    
    % Display first few from each tag
    maps:foreach(fun(Tag, TagEndpoints) ->
        io:format("~n  Tag: ~s (~p endpoints)~n", [Tag, length(TagEndpoints)]),
        lists:foreach(fun(Endpoint) ->
            Method = element(2, Endpoint), % #endpoint.method
            Path = element(3, Endpoint),   % #endpoint.path
            Summary = element(5, Endpoint), % #endpoint.summary
            io:format("    ~s ~s - ~s~n", [Method, Path, Summary])
        end, lists:sublist(TagEndpoints, 3))
    end, GroupedEndpoints),
    
    io:format("~n  Total endpoints registered: ~p~n", [length(Endpoints)]).

%% Integration with agents example
demo_agent_with_api_tools() ->
    io:format("~n5. Creating Agent with API Tools:~n"),
    
    % Create an agent that can use discovered API endpoints
    Config = #{
        name => <<"API Explorer">>,
        model => <<"gpt-4.1-mini">>,
        system_prompt => <<"You are an API explorer agent. You can discover and test API endpoints dynamically.">>,
        tools => [api_discovery, api_testing, endpoint_registry_access]
    },
    
    case agent:create(Config) of
        {ok, AgentId} ->
            io:format("  Created API Explorer agent: ~p~n", [AgentId]),
            
            % Ask the agent to explore available endpoints
            {ok, Response} = agent:execute(AgentId, #{
                action => <<"chat">>,
                message => <<"What OpenAI API endpoints are available for chat completions?">>
            }),
            
            io:format("  Agent response: ~s~n", [Response]);
        {error, Reason} ->
            io:format("  Failed to create agent: ~p~n", [Reason])
    end.