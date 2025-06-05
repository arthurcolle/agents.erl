#!/usr/bin/env escript

main(_) ->
    % Add the built applications to the code path
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    
    % Start required applications  
    application:ensure_all_started(ssl),
    application:ensure_all_started(gun),
    application:ensure_all_started(jsx),
    application:ensure_all_started(uuid),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(openai),
    application:ensure_all_started(agents),
    
    io:format("Testing Agent Streaming Capabilities...~n"),
    
    % Start the web application
    case application:start(agent_web) of
        ok ->
            io:format("✓ Agent web application started successfully~n"),
            
            % Wait a moment for services to initialize
            timer:sleep(3000),
            
            % Test creating an agent via HTTP API
            test_agent_creation_http(),
            
            % Test streaming execution via WebSocket
            test_streaming_websocket(),
            
            % Stop the application
            application:stop(agent_web),
            io:format("✓ Agent web application stopped~n");
        {error, Reason} ->
            io:format("✗ Failed to start agent web application: ~p~n", [Reason])
    end,
    
    io:format("~n=== Agent Streaming Test Complete ===~n").

test_agent_creation_http() ->
    io:format("~n--- Testing Agent Creation via HTTP API ---~n"),
    
    % Create agent via HTTP POST to /api/agents
    RequestBody = jsx:encode(#{
        type => <<"simple">>,
        name => <<"Test Streaming Agent">>,
        tools => []
    }),
    
    case httpc:request(post, 
        {"http://localhost:8080/api/agents", 
         [{"content-type", "application/json"}],
         "application/json", 
         binary_to_list(RequestBody)}, 
        [], []) of
        {ok, {{_, 201, _}, _Headers, ResponseBody}} ->
            try jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"id">> := AgentId} = Response ->
                    io:format("✓ Agent created successfully with ID: ~s~n", [AgentId]),
                    io:format("Response: ~p~n", [Response]),
                    {ok, AgentId};
                _ ->
                    io:format("✗ Invalid response format: ~s~n", [ResponseBody]),
                    {error, invalid_response}
            catch
                _:_ ->
                    io:format("✗ Failed to parse response: ~s~n", [ResponseBody]),
                    {error, parse_error}
            end;
        {ok, {{_, StatusCode, _}, _Headers, ErrorBody}} ->
            io:format("✗ HTTP request failed with status ~p: ~s~n", [StatusCode, ErrorBody]),
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            io:format("✗ HTTP request failed: ~p~n", [Reason]),
            {error, Reason}
    end.

test_streaming_websocket() ->
    io:format("~n--- Testing WebSocket Streaming ---~n"),
    
    % First create an agent
    case test_agent_creation_http() of
        {ok, AgentId} ->
            io:format("Testing WebSocket connection to agent ~s~n", [AgentId]),
            
            % Try to connect to WebSocket endpoint
            case gun:open("localhost", 8080) of
                {ok, ConnPid} ->
                    case gun:await_up(ConnPid, 5000) of
                        {ok, _Protocol} ->
                            io:format("✓ Connected to WebSocket server~n"),
                            
                            % Upgrade to WebSocket
                            StreamRef = gun:ws_upgrade(ConnPid, "/ws"),
                            case gun:await(ConnPid, StreamRef, 5000) of
                                {upgrade, [<<"websocket">>], _Headers} ->
                                    io:format("✓ WebSocket upgrade successful~n"),
                                    
                                    % Send a message to test streaming
                                    Message = jsx:encode(#{
                                        type => <<"execute_agent">>,
                                        agent_id => AgentId,
                                        message => <<"Hello, please respond with a simple greeting.">>
                                    }),
                                    gun:ws_send(ConnPid, StreamRef, {text, Message}),
                                    
                                    % Listen for streaming responses
                                    listen_for_stream_messages(ConnPid, 10000),
                                    
                                    gun:close(ConnPid);
                                {error, Reason} ->
                                    io:format("✗ WebSocket upgrade failed: ~p~n", [Reason]),
                                    gun:close(ConnPid)
                            end;
                        {error, Reason} ->
                            io:format("✗ Failed to connect: ~p~n", [Reason]),
                            gun:close(ConnPid)
                    end;
                {error, Reason} ->
                    io:format("✗ Failed to open connection: ~p~n", [Reason])
            end,
            
            % Clean up the test agent
            cleanup_agent(AgentId);
        {error, _} ->
            io:format("✗ Cannot test streaming - agent creation failed~n")
    end.

listen_for_stream_messages(ConnPid, Timeout) ->
    receive
        {gun_ws, ConnPid, _StreamRef, {text, Data}} ->
            try jsx:decode(Data, [return_maps]) of
                Message ->
                    io:format("STREAM MESSAGE: ~p~n", [Message]),
                    case maps:get(<<"type">>, Message, undefined) of
                        <<"streaming_end">> ->
                            io:format("✓ Streaming completed~n");
                        <<"error">> ->
                            io:format("✗ Stream error: ~p~n", [Message]);
                        _ ->
                            listen_for_stream_messages(ConnPid, Timeout)
                    end
            catch
                _:_ ->
                    io:format("STREAM DATA: ~s~n", [Data]),
                    listen_for_stream_messages(ConnPid, Timeout)
            end;
        {gun_ws, ConnPid, _StreamRef, close} ->
            io:format("✓ WebSocket closed~n");
        Other ->
            io:format("OTHER MESSAGE: ~p~n", [Other]),
            listen_for_stream_messages(ConnPid, Timeout)
    after Timeout ->
        io:format("⚠ Stream timeout after ~p ms~n", [Timeout])
    end.

cleanup_agent(AgentId) ->
    case httpc:request(delete, 
        {"http://localhost:8080/api/agents/" ++ binary_to_list(AgentId), []}, 
        [], []) of
        {ok, {{_, 204, _}, _Headers, _}} ->
            io:format("✓ Agent cleaned up successfully~n");
        _ ->
            io:format("⚠ Failed to cleanup agent~n")
    end.