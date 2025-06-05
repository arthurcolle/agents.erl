#!/usr/bin/env escript
%% test_minimal_supervisor.erl
%% Test the minimal supervisor works

main(_Args) ->
    io:format("🧪 Testing Minimal Supervisor~n"),
    io:format("============================~n~n"),
    
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
    
    %% Start applications
    io:format("🚀 Starting required applications...~n"),
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowboy),
    
    %% Test supervisor module
    io:format("✅ Testing supervisor module loading...~n"),
    case code:ensure_loaded(agent_web_sup) of
        {module, agent_web_sup} ->
            io:format("   ✅ agent_web_sup loaded~n");
        {error, Error} ->
            io:format("   ❌ Failed to load agent_web_sup: ~p~n", [Error]),
            halt(1)
    end,
    
    %% Test handler modules
    Handlers = [
        agent_web_handler,
        system_health_handler,
        hot_reload_handler,
        conversation_handler,
        agent_api_handler,
        agent_chat_handler,
        agent_ws_handler
    ],
    
    io:format("🔍 Testing handler modules...~n"),
    lists:foreach(fun(Handler) ->
        io:format("   ~p: ", [Handler]),
        case code:ensure_loaded(Handler) of
            {module, Handler} ->
                io:format("✅~n");
            {error, HandlerError} ->
                io:format("❌ ~p~n", [HandlerError])
        end
    end, Handlers),
    
    %% Test supervisor start
    io:format("~n🎯 Testing supervisor startup...~n"),
    try
        case agent_web_sup:start_link() of
            {ok, Pid} ->
                io:format("   ✅ Supervisor started with PID ~p~n", [Pid]),
                
                %% Test if it's supervising children
                Children = supervisor:which_children(agent_web_sup),
                io:format("   📊 Children: ~p~n", [length(Children)]),
                lists:foreach(fun({Id, ChildPid, Type, Modules}) ->
                    Status = case is_pid(ChildPid) andalso is_process_alive(ChildPid) of
                        true -> "✅ Running";
                        false -> "❌ Not running"
                    end,
                    io:format("      ~p: ~s (~p)~n", [Id, Status, ChildPid])
                end, Children),
                
                %% Clean up
                supervisor:terminate_child(agent_web_sup, agent_web_http_listener),
                exit(Pid, normal),
                
                io:format("~n🎉 SUCCESS: Minimal supervisor works!~n");
            {error, Reason} ->
                io:format("   ❌ Supervisor failed to start: ~p~n", [Reason])
        end
    catch
        ErrorType:ErrorReason ->
            io:format("   ❌ Exception during supervisor test: ~p:~p~n", [ErrorType, ErrorReason])
    end,
    
    io:format("~n✅ Minimal supervisor test complete!~n").