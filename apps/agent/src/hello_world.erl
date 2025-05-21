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
    % We'll just print a hello world message without requiring the application
    io:format("Hello World Example~n"),
    io:format("Message: ~s~n", [Message]),
    io:format("~nResponse: Hello from Erlang Agent Framework!~n"),
    ok.