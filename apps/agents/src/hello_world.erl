%% hello_world.erl
%% A simple hello world example for the Erlang agent framework
-module(hello_world).

%% API exports
-export([
    run/0,
    run/1
]).

%% @doc Run the hello world example with default message
-spec run() -> ok.
run() ->
    run(<<"Hello, World!">>).

%% @doc Run the hello world example with a custom message
-spec run(binary()) -> ok.
run(Message) ->
    % Start required applications
    application:ensure_all_started(jsx),
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    
    % Start OpenAI app
    case application:ensure_all_started(openai) of
        {ok, _} -> ok;
        Error -> 
            io:format("Failed to start openai app: ~p~n", [Error]),
            halt(1)
    end,
    
    % Send the chat message using simple_agent with streaming
    io:format("Hello World Example~n"),
    io:format("Message: ~s~n", [Message]),
    io:format("~nResponse: "),
    
    % Use streaming chat for real-time response
    case simple_agent:chat_stream(Message) of
        ok ->
            ok;
        {error, Reason} ->
            io:format("~nError: ~p~n", [Reason])
    end,
    ok.