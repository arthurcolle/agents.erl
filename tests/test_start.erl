-module(test_start).
-export([start/0]).

start() ->
    io:format("Setting up code path...~n"),
    
    % Add application directories to code path
    code:add_patha("ebin"),
    code:add_patha("_build/default/lib/agent/ebin"),
    code:add_patha("_build/default/lib/openai/ebin"),
    code:add_patha("_build/default/lib/jsx/ebin"),
    
    io:format("Code path: ~p~n", [code:get_path()]),
    
    io:format("Starting agent application...~n"),
    
    % Start the application
    case application:ensure_all_started(agent) of
        {ok, StartedApps} ->
            io:format("Successfully started applications: ~p~n", [StartedApps]);
        {error, {App, Reason}} ->
            io:format("Failed to start application ~p: ~p~n", [App, Reason])
    end,
    
    % Check if processes are running
    io:format("~n== Process Status ==~n"),
    CheckProcess = fun(Name) ->
        case whereis(Name) of
            undefined -> 
                io:format("~p: NOT RUNNING~n", [Name]);
            Pid -> 
                io:format("~p: RUNNING (pid: ~p)~n", [Name, Pid])
        end
    end,
    
    CheckProcess(agent),
    CheckProcess(agent_registry),
    CheckProcess(agent_tools),
    CheckProcess(openai_sup),
    CheckProcess(openai_generator),
    CheckProcess(openai_clients_sup),
    
    io:format("~nStartup test completed.~n"),
    ok.