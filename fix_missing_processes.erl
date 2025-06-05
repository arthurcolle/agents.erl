#!/usr/bin/env escript
%% fix_missing_processes.erl
%% Manually start the missing processes to fix the health issues

main(_Args) ->
    io:format("🔧 Fixing Missing Processes~n"),
    io:format("===========================~n~n"),
    
    %% Connect to the running system
    Node = 'agent_web@localhost',
    case net_adm:ping(Node) of
        pong ->
            io:format("✅ Connected to running system: ~p~n", [Node]);
        pang ->
            %% Try to connect to node without hostname
            AltNodes = ['agent_web@agent-matrix.local', agent_web],
            Connected = lists:any(fun(N) ->
                case net_adm:ping(N) of
                    pong -> 
                        io:format("✅ Connected to running system: ~p~n", [N]),
                        true;
                    pang -> false
                end
            end, AltNodes),
            
            case Connected of
                false ->
                    io:format("❌ Cannot connect to running system, trying local startup~n"),
                    start_processes_locally()
            end
    end,
    
    %% Start missing processes
    MissingProcesses = [
        mcp_registry,
        mcp_connection_manager,
        mcp_manager,
        mcp_advanced_config,
        oauth_manager,
        mcp_monitor
    ],
    
    io:format("~n🚀 Starting missing processes:~n"),
    lists:foreach(fun(Process) ->
        io:format("   Starting ~p: ", [Process]),
        try
            case Process:start_link() of
                {ok, Pid} ->
                    io:format("✅ PID ~p~n", [Pid]);
                {error, {already_started, Pid}} ->
                    io:format("✅ Already running with PID ~p~n", [Pid]);
                {error, Reason} ->
                    io:format("❌ Error: ~p~n", [Reason])
            end
        catch
            Error:Reason ->
                io:format("❌ Exception: ~p:~p~n", [Error, Reason])
        end
    end, MissingProcesses),
    
    io:format("~n✅ Process startup complete!~n").

start_processes_locally() ->
    io:format("Starting processes in local environment~n"),
    
    %% Add paths
    code:add_paths([
        "_build/default/lib/agents/ebin", 
        "_build/default/lib/openai/ebin", 
        "_build/default/lib/agent_web/ebin",
        "_build/default/lib/jsx/ebin",
        "_build/default/lib/cowboy/ebin",
        "_build/default/lib/cowlib/ebin",
        "_build/default/lib/ranch/ebin"
    ]),
    ok.