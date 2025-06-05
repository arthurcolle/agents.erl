#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin -Wall

-module(demo_ai_error_interpretation).

main(_) ->
    io:format("~n=== AI Error Interpretation Demo ===~n~n"),
    
    %% Start the applications
    application:ensure_all_started(agent_web),
    timer:sleep(2000),
    
    %% Demonstrate various error scenarios
    demo_error_scenarios(),
    
    io:format("~n=== Demo Complete ===~n"),
    io:format("Visit http://localhost:8080 and click on 'AI Insights' tab to see interpretations~n~n").

demo_error_scenarios() ->
    io:format("1. Demonstrating badarg error...~n"),
    catch demo_badarg(),
    timer:sleep(1000),
    
    io:format("~n2. Demonstrating badmatch error...~n"),
    catch demo_badmatch(),
    timer:sleep(1000),
    
    io:format("~n3. Demonstrating process crash...~n"),
    demo_process_crash(),
    timer:sleep(1000),
    
    io:format("~n4. Demonstrating supervisor failure...~n"),
    demo_supervisor_failure(),
    timer:sleep(1000),
    
    io:format("~n5. Demonstrating timeout error...~n"),
    demo_timeout(),
    timer:sleep(1000),
    
    io:format("~n6. Demonstrating custom error logging...~n"),
    demo_custom_errors(),
    timer:sleep(1000).

%% Badarg error
demo_badarg() ->
    error_logger:error_msg("Attempting to convert invalid data to atom~n"),
    %% This will cause a badarg
    binary_to_atom(<<255, 255, 255>>, utf8).

%% Badmatch error
demo_badmatch() ->
    error_logger:info_msg("Attempting pattern match that will fail~n"),
    Data = {ok, unexpected_value},
    %% This will cause a badmatch
    {error, Reason} = Data,
    io:format("Reason: ~p~n", [Reason]).

%% Process crash
demo_process_crash() ->
    error_logger:info_msg("Starting process that will crash~n"),
    spawn_link(fun() ->
        register(demo_crash_process, self()),
        timer:sleep(500),
        %% Simulate a crash
        1 / 0
    end).

%% Supervisor failure
demo_supervisor_failure() ->
    error_logger:info_msg("Creating supervisor with failing child~n"),
    
    ChildSpec = #{
        id => failing_child,
        start => {erlang, apply, [fun() -> 
            error_logger:error_msg("Child process failing to start~n"),
            {error, deliberate_failure}
        end, []]},
        restart => permanent,
        shutdown => 5000,
        type => worker
    },
    
    supervisor:start_link(
        {local, demo_failing_sup},
        ?MODULE,
        [ChildSpec]
    ).

%% Timeout error
demo_timeout() ->
    error_logger:info_msg("Demonstrating gen_server timeout~n"),
    
    %% Create a gen_server that will timeout
    {ok, Pid} = gen_server:start_link(
        {local, demo_timeout_server},
        demo_timeout_server,
        [],
        []
    ),
    
    %% This will timeout
    catch gen_server:call(Pid, slow_operation, 100).

%% Custom error logging
demo_custom_errors() ->
    %% Various error patterns
    error_logger:error_msg("Database connection failed: ~p~n", [{error, econnrefused}]),
    error_logger:error_msg("Out of memory error in process ~p~n", [self()]),
    error_logger:warning_msg("System limit reached: ~p~n", [{system_limit, process_limit}]),
    error_logger:error_msg("Critical: Port limit exceeded, cannot open more connections~n"),
    error_logger:error_msg("Function clause error in module ~p, function ~p/~p~n", 
                          [my_module, my_function, 3]),
    
    %% Log with complex error data
    ComplexError = {error, {badarg, [
        {erlang, binary_to_term, [<<131,100,0,5,104,101,108,108,111>>], []},
        {my_module, decode_message, 2, [{file, "my_module.erl"}, {line, 42}]},
        {my_module, handle_call, 3, [{file, "my_module.erl"}, {line, 156}]}
    ]}},
    
    error_logger:error_msg("Complex error occurred: ~p~n", [ComplexError]).

%% Supervisor init callback
init([ChildSpec]) ->
    {ok, {{one_for_one, 1, 1}, [ChildSpec]}}.

%% Demo gen_server for timeout
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(demo_timeout_server, {}).

init([]) ->
    {ok, #demo_timeout_server{}}.

handle_call(slow_operation, _From, State) ->
    %% Simulate slow operation
    timer:sleep(5000),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.