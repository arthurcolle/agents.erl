#!/usr/bin/env escript

%% Simple test for autonomous multi-turn function calling
-module(test_autonomous_simple).
-mode(compile).

-export([main/1]).

main(_) ->
    % Add paths for compiled modules  
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required applications
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),
    
    io:format("=== Simple Autonomous Function Calling Test ===~n"),
    
    try
        test_agent_creation_and_autonomous_mode(),
        io:format("~n✅ Simple autonomous test completed successfully!~n")
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n~p~n", [Class, Reason, Stacktrace]),
            halt(1)
    end.

test_agent_creation_and_autonomous_mode() ->
    io:format("~n🤖 Testing Agent Creation with Autonomous Capabilities...~n"),
    
    % Create a simple agent configuration
    AgentConfig = #{
        id => <<"simple-autonomous-agent">>,
        name => <<"Simple Autonomous Agent">>,
        type => ai,
        model => <<"gpt-4.1-mini">>,
        tools => [shell, who_am_i, get_system_state],
        system_prompt => <<"You are an autonomous agent for testing. Use available tools when appropriate.">>,
        autonomous_mode => false,
        max_autonomous_turns => 3
    },
    
    % Start the agent
    {ok, AgentPid} = agent_instance:start_link(AgentConfig),
    io:format("✅ Agent created successfully~n"),
    
    % Test getting state
    StateResult = agent_instance:get_state(AgentPid),
    case StateResult of
        {ok, State} -> 
            io:format("✅ Agent state retrieved: ~p~n", [maps:get(id, State)]);
        State when is_map(State) ->
            io:format("✅ Agent state retrieved: ~p~n", [maps:get(id, State)]);
        Other ->
            throw({error, {unexpected_state_format, Other}})
    end,
    
    % Test enabling autonomous mode
    ok = agent_instance:enable_autonomous_mode(AgentPid),
    EnabledStateResult = agent_instance:get_state(AgentPid),
    EnabledState = case EnabledStateResult of
        {ok, S} -> S;
        S when is_map(S) -> S;
        _ -> throw({error, could_not_get_enabled_state})
    end,
    case maps:get(autonomous_mode, EnabledState) of
        true -> io:format("✅ Autonomous mode enabled~n");
        false -> throw({error, autonomous_mode_not_enabled})
    end,
    
    % Test sending a self-message
    agent_instance:self_message(AgentPid, <<"Test autonomous operation">>),
    io:format("✅ Self-message sent~n"),
    
    % Wait briefly for async processing
    timer:sleep(500),
    
    % Test disabling autonomous mode
    ok = agent_instance:disable_autonomous_mode(AgentPid),
    io:format("✅ Autonomous mode disabled~n"),
    
    % Clean up
    gen_server:stop(AgentPid),
    io:format("✅ Agent stopped~n"),
    
    ok.