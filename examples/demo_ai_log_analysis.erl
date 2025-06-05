#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa apps/*/ebin

-mode(compile).

main([]) ->
    io:format("~n=== AI-Powered Log Analysis Demo ===~n~n"),
    
    %% Start required applications
    start_apps(),
    
    %% Wait for services to initialize
    timer:sleep(2000),
    
    %% Run demonstrations
    demo_timeout_analysis(),
    timer:sleep(2000),
    
    demo_error_batch_analysis(),
    timer:sleep(2000),
    
    demo_periodic_analysis(),
    timer:sleep(2000),
    
    demo_realtime_monitoring(),
    
    io:format("~n=== Demo Complete ===~n"),
    init:stop().

start_apps() ->
    io:format("Starting applications...~n"),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(jsx),
    application:start(hackney),
    
    %% Set OpenAI API key from environment
    case os:getenv("OPENAI_API_KEY") of
        false ->
            io:format("WARNING: OPENAI_API_KEY not set. AI features will not work.~n");
        Key ->
            application:set_env(openai, api_key, Key)
    end,
    
    %% Start our applications
    application:start(openai),
    application:start(agents),
    application:start(agent_web),
    
    %% Enable AI log analysis
    ai_log_integration:enable_ai_analysis(),
    io:format("AI log analysis enabled.~n~n").

demo_timeout_analysis() ->
    io:format("~n--- Demonstrating Timeout Analysis ---~n"),
    
    %% Simulate a gen_server timeout
    spawn(fun() ->
        %% This will generate a timeout error
        try
            gen_server:call(non_existent_server, test, 1000)
        catch
            exit:{timeout, _} ->
                %% Log the timeout
                error_logger:error_msg("gen_server call timeout occurred when trying to reach non_existent_server~n"),
                
                %% Manually trigger analysis
                TimeoutInfo = #{
                    type => gen_server_timeout,
                    duration_ms => 1000,
                    target => non_existent_server,
                    operation => call,
                    stacktrace => erlang:get_stacktrace()
                },
                ai_timeout_analyzer:analyze_timeout(TimeoutInfo, #{
                    module => demo,
                    function => demo_timeout_analysis,
                    line => 42
                })
        end
    end),
    
    %% Wait for analysis
    timer:sleep(5000),
    
    %% Get analysis report
    case ai_timeout_analyzer:get_analysis_report() of
        {ok, Report} ->
            io:format("Timeout Analysis Report:~n"),
            print_report(Report);
        Error ->
            io:format("Failed to get report: ~p~n", [Error])
    end.

demo_error_batch_analysis() ->
    io:format("~n--- Demonstrating Error Batch Analysis ---~n"),
    
    %% Generate multiple errors for batch analysis
    lists:foreach(fun(N) ->
        spawn(fun() ->
            %% Different error types
            case N rem 3 of
                0 ->
                    %% badarg error
                    try
                        list_to_atom(123)
                    catch
                        error:badarg ->
                            error_logger:error_msg("badarg error in demo ~p: invalid argument to list_to_atom~n", [N])
                    end;
                1 ->
                    %% badmatch error
                    try
                        {ok, Value} = {error, demo_error},
                        Value
                    catch
                        error:{badmatch, _} ->
                            error_logger:error_msg("badmatch error in demo ~p: pattern matching failed~n", [N])
                    end;
                2 ->
                    %% function_clause error
                    try
                        demo_function("invalid")
                    catch
                        error:function_clause ->
                            error_logger:error_msg("function_clause error in demo ~p: no matching function clause~n", [N])
                    end
            end
        end)
    end, lists:seq(1, 15)),
    
    %% Wait for errors to be processed
    timer:sleep(3000),
    
    %% Trigger batch analysis
    ai_timeout_analyzer:analyze_significant_error(#{
        type => batch_analysis_trigger,
        message => <<"Multiple errors detected, triggering batch analysis">>
    }, #{source => demo}).

demo_periodic_analysis() ->
    io:format("~n--- Demonstrating Periodic Analysis ---~n"),
    
    %% Configure periodic analysis for demo (every 10 seconds)
    ai_log_integration:configure(#{periodic_interval => 10000}),
    
    io:format("Periodic analysis configured for every 10 seconds.~n"),
    io:format("Generating some activity...~n"),
    
    %% Generate various log events
    lists:foreach(fun(N) ->
        case N rem 5 of
            0 -> error_logger:warning_msg("Warning message ~p: Resource usage high~n", [N]);
            1 -> error_logger:info_msg("Info message ~p: Processing request~n", [N]);
            2 -> logger:error("Error message ~p: Connection failed", [N]);
            3 -> logger:warning("Warning ~p: Slow query detected", [N]);
            _ -> logger:info("Info ~p: Task completed", [N])
        end,
        timer:sleep(500)
    end, lists:seq(1, 20)),
    
    io:format("Waiting for periodic analysis to trigger...~n"),
    timer:sleep(12000).

demo_realtime_monitoring() ->
    io:format("~n--- Demonstrating Real-time Monitoring ---~n"),
    
    %% Subscribe to analysis events
    Self = self(),
    ai_timeout_analyzer:subscribe(Self),
    ai_error_interpreter:subscribe(Self),
    
    io:format("Subscribed to real-time analysis events.~n"),
    io:format("Generating errors with timeouts...~n"),
    
    %% Generate a timeout that will be analyzed in real-time
    spawn(fun() ->
        try
            %% Simulate HTTP timeout
            error_logger:error_msg("HTTP request timeout: Server did not respond within 30000ms~n"
                                   "URL: https://api.example.com/data~n"
                                   "Method: GET~n"
                                   "Timeout: 30000ms~n"),
            
            %% Simulate TCP timeout
            timer:sleep(1000),
            error_logger:error_msg("TCP connection timeout to 192.168.1.100:5432~n"
                                   "Connection attempt failed after 5000ms~n")
        catch
            _:_ -> ok
        end
    end),
    
    %% Wait for and display real-time events
    io:format("Listening for analysis events (10 seconds)...~n"),
    receive_events(10000).

demo_function(atom) -> ok.

print_report(Report) when is_map(Report) ->
    maps:foreach(fun(Key, Value) ->
        io:format("  ~p: ~p~n", [Key, format_value(Value)])
    end, Report).

format_value(Value) when is_list(Value), length(Value) > 3 ->
    io_lib:format("~p items", [length(Value)]);
format_value(Value) ->
    Value.

receive_events(Timeout) ->
    receive
        {timeout_analysis_event, {new_analysis, Analysis}} ->
            io:format("~n[REAL-TIME] Timeout Analysis Received:~n"),
            io:format("  Type: ~p~n", [maps:get(timeout_type, Analysis, unknown)]),
            io:format("  Severity: ~p~n", [maps:get(severity, Analysis, unknown)]),
            io:format("  Interpretation: ~s~n", [maps:get(interpretation, Analysis, "")]),
            io:format("  Root Cause: ~s~n", [maps:get(root_cause, Analysis, "")]),
            receive_events(Timeout - 1000);
            
        {error_interpretation_event, {new_interpretation, Interp}} ->
            io:format("~n[REAL-TIME] Error Interpretation Received:~n"),
            io:format("  Category: ~s~n", [maps:get(category, Interp, "")]),
            io:format("  Severity: ~p~n", [maps:get(severity, Interp, unknown)]),
            io:format("  Interpretation: ~s~n", [maps:get(interpretation, Interp, "")]),
            lists:foreach(fun(Suggestion) ->
                io:format("  - ~s~n", [Suggestion])
            end, maps:get(suggestions, Interp, [])),
            receive_events(Timeout - 1000);
            
        {timeout_analysis_event, {periodic_analysis, Analysis}} ->
            io:format("~n[PERIODIC] System Analysis Received:~n"),
            io:format("  ~s~n", [maps:get(analysis, Analysis, "No analysis available")]),
            receive_events(Timeout - 1000)
            
    after Timeout ->
        io:format("~nNo more events received.~n")
    end.