%% Simple test to identify core issues
-module(test_basic_agent).
-export([test_basic_functionality/0]).

test_basic_functionality() ->
    io:format("Testing basic agent functionality...~n"),
    
    % Test 1: Can we start the applications?
    io:format("1. Starting crypto...~n"),
    crypto:start(),
    
    io:format("2. Starting jsx...~n"),
    application:start(jsx),
    
    io:format("3. Starting agents app...~n"),
    case application:start(agents) of
        ok -> 
            io:format("✓ Agents app started successfully~n");
        {error, {already_started, _}} ->
            io:format("✓ Agents app already running~n");
        Error ->
            io:format("✗ Agents app failed to start: ~p~n", [Error])
    end,
    
    % Test 2: Can we start an agent instance?
    io:format("4. Testing agent instance creation...~n"),
    case agent_supervisor:start_agent(#{
        id => <<"test-agent">>,
        name => <<"Test Agent">>,
        type => simple,
        system_prompt => <<"You are a test agent">>
    }) of
        {ok, AgentPid} ->
            io:format("✓ Agent instance created: ~p~n", [AgentPid]),
            
            % Test 3: Can we execute a simple action?
            io:format("5. Testing agent execution...~n"),
            case agent_instance:execute(AgentPid, #{
                action => <<"chat">>,
                message => <<"Hello, test!">>
            }) of
                {ok, Response} ->
                    io:format("✓ Agent responded: ~p~n", [Response]);
                Error2 ->
                    io:format("✗ Agent execution failed: ~p~n", [Error2])
            end;
        Error1 ->
            io:format("✗ Agent instance creation failed: ~p~n", [Error1])
    end,
    
    io:format("Basic test completed.~n").