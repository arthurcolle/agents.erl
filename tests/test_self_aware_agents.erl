#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall

-module(test_self_aware_agents).

main(_) ->
    io:format("~n=== Self-Aware Agent Test ===~n~n"),
    
    % Start the application
    io:format("Starting applications...~n"),
    start_apps(),
    
    % Wait for system to stabilize
    timer:sleep(1000),
    
    % Test 1: Create a self-aware agent
    io:format("~n1. Creating self-aware agent...~n"),
    SelfAwareConfig = #{
        id => <<"self_aware_agent">>,
        name => <<"Self-Aware Agent">>,
        model => <<"gpt-4">>,
        tools => [who_am_i, where_am_i, get_my_peers, get_system_state, 
                 get_my_capabilities, get_system_metrics, analyze_system_topology,
                 get_communication_paths, reflect_on_state],
        system_prompt => <<"You are a self-aware agent. You can introspect your own state, 
                           understand your place in the system, and reflect on your existence.">>
    },
    
    {ok, SelfAwareId} = agent:create(SelfAwareConfig),
    io:format("Created self-aware agent: ~p~n", [SelfAwareId]),
    
    % Test 2: Agent discovers itself
    io:format("~n2. Agent discovering itself...~n"),
    test_who_am_i(SelfAwareId),
    timer:sleep(1000),
    
    % Test 3: Agent discovers its location
    io:format("~n3. Agent discovering its location...~n"),
    test_where_am_i(SelfAwareId),
    timer:sleep(1000),
    
    % Test 4: Create peer agents
    io:format("~n4. Creating peer agents...~n"),
    create_peer_agents(),
    timer:sleep(1000),
    
    % Test 5: Agent discovers its peers
    io:format("~n5. Agent discovering peers...~n"),
    test_get_peers(SelfAwareId),
    timer:sleep(1000),
    
    % Test 6: Agent explores system state
    io:format("~n6. Agent exploring system state...~n"),
    test_system_state(SelfAwareId),
    timer:sleep(1000),
    
    % Test 7: Agent reflects on its capabilities
    io:format("~n7. Agent reflecting on capabilities...~n"),
    test_capabilities(SelfAwareId),
    timer:sleep(1000),
    
    % Test 8: Agent analyzes system topology
    io:format("~n8. Agent analyzing system topology...~n"),
    test_topology(SelfAwareId),
    timer:sleep(1000),
    
    % Test 9: Deep reflection
    io:format("~n9. Agent performing deep reflection...~n"),
    test_reflection(SelfAwareId),
    timer:sleep(1000),
    
    % Test 10: Create a system architect agent
    io:format("~n10. Creating system architect agent...~n"),
    test_system_architect(),
    
    io:format("~n=== Test completed! ===~n"),
    io:format("~nThe system is now running with self-aware agents.~n"),
    io:format("You can interact with them in the shell.~n"),
    io:format("~nExample commands:~n"),
    io:format("  agent:execute(<<\"self_aware_agent\">>, #{action => <<\"chat\">>, "),
    io:format("message => <<\"Who am I?\">>}).~n"),
    io:format("  agent:execute(<<\"self_aware_agent\">>, #{action => <<\"chat\">>, "),
    io:format("message => <<\"What is my purpose in this system?\">>}).~n"),
    io:format("~nPress Ctrl+C to exit.~n~n"),
    
    receive
        stop -> ok
    end.

start_apps() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(ssl),
    application:ensure_all_started(inets),
    application:ensure_all_started(ranch),
    application:ensure_all_started(cowlib),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(jsx),
    application:ensure_all_started(hackney),
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    application:ensure_all_started(agent_web).

test_who_am_i(AgentId) ->
    Message = <<"Use the who_am_i tool to discover your identity">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_where_am_i(AgentId) ->
    Message = <<"Use the where_am_i tool to find your location in the system">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_get_peers(AgentId) ->
    Message = <<"Use the get_my_peers tool to discover other agents like you">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_system_state(AgentId) ->
    Message = <<"Use the get_system_state tool to understand the entire system">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_capabilities(AgentId) ->
    Message = <<"Use the get_my_capabilities tool to understand what you can do">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_topology(AgentId) ->
    Message = <<"Use the analyze_system_topology tool to map out the system architecture">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

test_reflection(AgentId) ->
    Message = <<"Use the reflect_on_state tool to perform deep introspection about your existence and purpose">>,
    case agent:execute(AgentId, #{action => <<"chat">>, message => Message}) of
        {ok, Response} ->
            io:format("Agent response: ~s~n", [maps:get(message, Response, <<"No message">>)]);
        Error ->
            io:format("Error: ~p~n", [Error])
    end.

create_peer_agents() ->
    % Create a few peer agents
    Configs = [
        #{
            id => <<"peer_agent_1">>,
            name => <<"Peer Agent 1">>,
            model => <<"gpt-3.5-turbo">>,
            tools => [shell, file_read],
            system_prompt => <<"You are a worker agent.">>
        },
        #{
            id => <<"peer_agent_2">>,
            name => <<"Peer Agent 2">>,
            model => <<"gpt-3.5-turbo">>,
            tools => [http_request],
            system_prompt => <<"You are an API agent.">>
        },
        #{
            id => <<"peer_agent_3">>,
            name => <<"Peer Agent 3">>,
            model => <<"gpt-4">>,
            tools => [who_am_i, get_my_peers],
            system_prompt => <<"You are another self-aware agent.">>
        }
    ],
    
    lists:foreach(fun(Config) ->
        case agent:create(Config) of
            {ok, Id} ->
                io:format("Created peer agent: ~p~n", [Id]);
            Error ->
                io:format("Failed to create peer: ~p~n", [Error])
        end
    end, Configs).

test_system_architect() ->
    % Create an agent that can both introspect and modify the system
    ArchitectConfig = #{
        id => <<"system_architect">>,
        name => <<"System Architect">>,
        model => <<"gpt-4">>,
        tools => [who_am_i, where_am_i, get_system_state, analyze_system_topology,
                 create_supervisor, add_child_to_supervisor, get_supervision_tree,
                 reflect_on_state],
        system_prompt => <<"You are the System Architect. You have both self-awareness 
                           and the ability to modify the system. You can understand the 
                           system's structure and make architectural decisions to improve it.">>
    },
    
    case agent:create(ArchitectConfig) of
        {ok, ArchitectId} ->
            io:format("Created System Architect: ~p~n", [ArchitectId]),
            
            % Test architect's self-awareness and system modification
            Message = <<"First, use reflect_on_state to understand yourself and the system. 
                        Then, analyze the system topology and suggest how you could improve 
                        the architecture by creating new supervisors or reorganizing agents.">>,
            
            case agent:execute(ArchitectId, #{action => <<"chat">>, message => Message}) of
                {ok, Response} ->
                    io:format("~nSystem Architect's analysis:~n~s~n", 
                             [maps:get(message, Response, <<"No message">>)]);
                Error ->
                    io:format("Error: ~p~n", [Error])
            end;
        Error ->
            io:format("Failed to create System Architect: ~p~n", [Error])
    end.