#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall

-module(test_dynamic_supervisors).

main(_) ->
    io:format("~n=== Dynamic Supervisor Test ===~n~n"),
    
    % Start the application
    io:format("Starting applications...~n"),
    start_apps(),
    
    % Wait for system to stabilize
    timer:sleep(1000),
    
    % Test 1: Create a dynamic supervisor
    io:format("~n1. Creating dynamic supervisor 'test_sup'...~n"),
    runtime_system:create_sup(test_sup, one_for_one),
    
    % Test 2: List supervisors
    io:format("~n2. Listing all supervisors...~n"),
    runtime_system:list_sups(),
    
    % Test 3: Add children to the supervisor
    io:format("~n3. Adding children to test_sup...~n"),
    
    % Create agent configs
    AgentConfig1 = #{
        name => <<"Test Agent 1">>,
        model => <<"gpt-4.1-mini">>,
        tools => [shell, file_read],
        system_prompt => <<"You are a test agent.">>
    },
    
    AgentConfig2 = #{
        name => <<"Test Agent 2">>,
        model => <<"gpt-3.5-turbo">>,
        tools => [http_request],
        system_prompt => <<"You are another test agent.">>
    },
    
    % Spawn agents under the supervisor
    runtime_system:spawn_agent_under(test_sup, test_agent_1, AgentConfig1),
    runtime_system:spawn_agent_under(test_sup, test_agent_2, AgentConfig2),
    
    % Test 4: Get supervisor info
    io:format("~n4. Getting supervisor info...~n"),
    runtime_system:sup_info(test_sup),
    
    % Test 5: Create an agent group
    io:format("~n5. Creating agent group 'worker_pool'...~n"),
    GroupConfig = #{
        model => <<"gpt-3.5-turbo">>,
        tools => [shell],
        system_prompt => <<"You are a worker agent.">>
    },
    runtime_system:spawn_agent_group(worker_pool, 3, GroupConfig),
    
    % Test 6: Display supervision tree
    io:format("~n6. Displaying supervision tree...~n"),
    runtime_system:tree(),
    
    % Test 7: System health check
    io:format("~n7. Checking system health...~n"),
    runtime_system:health(),
    
    % Test 8: Test supervisor tools through agent
    io:format("~n8. Testing supervisor tools through an agent...~n"),
    test_supervisor_tools(),
    
    % Test 9: Augment system with a new service
    io:format("~n9. Augmenting system with new service...~n"),
    runtime_system:augment(custom_service, agent_instance, [#{
        name => <<"Custom Service Agent">>,
        model => <<"gpt-4.1-mini">>,
        tools => [create_supervisor, list_supervisors, get_supervision_tree],
        system_prompt => <<"You are a system management agent.">>
    }]),
    
    % Test 10: System inspection
    io:format("~n10. Inspecting the system...~n"),
    runtime_system:inspect(),
    
    % Wait a bit to see the system running
    timer:sleep(2000),
    
    % Test 11: Remove a child
    io:format("~n11. Removing a child from test_sup...~n"),
    runtime_system:remove_child(test_sup, test_agent_1),
    runtime_system:sup_info(test_sup),
    
    % Test 12: Stop a supervisor
    io:format("~n12. Stopping test_sup...~n"),
    runtime_system:stop_sup(test_sup),
    runtime_system:list_sups(),
    
    io:format("~n=== Test completed successfully! ===~n~n"),
    
    % Keep the system running for manual testing
    io:format("System is running. You can now interact with it in the shell.~n"),
    io:format("Example commands:~n"),
    io:format("  runtime_system:create_sup(my_sup, one_for_all).~n"),
    io:format("  runtime_system:spawn_agent_under(my_sup, my_agent, #{name => <<\"My Agent\">>}).~n"),
    io:format("  runtime_system:tree().~n"),
    io:format("  runtime_system:health().~n"),
    io:format("~nPress Ctrl+C to exit.~n~n"),
    
    receive
        stop -> ok
    end.

start_apps() ->
    % Start required applications
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowlib),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    
    % Start our applications
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web).

test_supervisor_tools() ->
    % Create an agent that can manage supervisors
    ManagerConfig = #{
        id => <<"supervisor_manager">>,
        name => <<"Supervisor Manager">>,
        model => <<"gpt-4.1-mini">>,
        tools => [create_supervisor, stop_supervisor, list_supervisors, 
                 add_child_to_supervisor, get_supervision_tree],
        system_prompt => <<"You are a supervisor management agent. You can create and manage supervisors dynamically.">>
    },
    
    case agent:create(ManagerConfig) of
        {ok, AgentId} ->
            io:format("Created supervisor manager agent: ~p~n", [AgentId]),
            
            % Test creating a supervisor through the agent
            TestMessage1 = <<"Create a new supervisor called 'dynamic_test_sup' with one_for_one strategy">>,
            case agent:execute(AgentId, #{action => <<"chat">>, message => TestMessage1}) of
                {ok, Response1} ->
                    io:format("Agent response: ~s~n", [maps:get(message, Response1, <<"No message">>)]);
                Error1 ->
                    io:format("Error: ~p~n", [Error1])
            end,
            
            % Test listing supervisors through the agent
            TestMessage2 = <<"List all active supervisors">>,
            case agent:execute(AgentId, #{action => <<"chat">>, message => TestMessage2}) of
                {ok, Response2} ->
                    io:format("Agent response: ~s~n", [maps:get(message, Response2, <<"No message">>)]);
                Error2 ->
                    io:format("Error: ~p~n", [Error2])
            end;
        Error ->
            io:format("Failed to create supervisor manager agent: ~p~n", [Error])
    end.