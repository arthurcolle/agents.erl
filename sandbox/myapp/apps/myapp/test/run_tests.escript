#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Test Runner Script
%%% Runs all test suites and generates reports
%%% @end
%%%-------------------------------------------------------------------

-define(TEST_MODULES, [
    agent_discovery_tests,
    agent_messenger_tests,
    agent_tools_tests,
    agent_protocol_tests,
    uuid_tests,
    integration_tests,
    property_tests,
    performance_tests
]).

main(Args) ->
    io:format("~n=== MyApp Test Suite ===~n~n"),
    
    % Parse arguments
    Options = parse_args(Args),
    
    % Ensure code paths are set
    setup_paths(),
    
    % Run tests based on options
    case maps:get(module, Options, all) of
        all ->
            run_all_tests(Options);
        Module when is_atom(Module) ->
            run_module_tests(Module, Options);
        _ ->
            io:format("Error: Invalid module specified~n"),
            usage()
    end.

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Opts) ->
    Opts;
parse_args(["-m", Module | Rest], Opts) ->
    parse_args(Rest, Opts#{module => list_to_atom(Module)});
parse_args(["-v" | Rest], Opts) ->
    parse_args(Rest, Opts#{verbose => true});
parse_args(["-p" | Rest], Opts) ->
    parse_args(Rest, Opts#{performance => true});
parse_args(["-q" | Rest], Opts) ->
    parse_args(Rest, Opts#{quick => true});
parse_args(["-h" | _], _) ->
    usage(),
    halt(0);
parse_args([Unknown | _], _) ->
    io:format("Unknown option: ~s~n", [Unknown]),
    usage(),
    halt(1).

usage() ->
    io:format("Usage: run_tests.escript [options]~n"),
    io:format("Options:~n"),
    io:format("  -m MODULE    Run tests for specific module~n"),
    io:format("  -v           Verbose output~n"),
    io:format("  -p           Include performance tests~n"),
    io:format("  -q           Quick mode (skip slow tests)~n"),
    io:format("  -h           Show this help~n"),
    io:format("~nAvailable test modules:~n"),
    lists:foreach(fun(M) ->
        io:format("  ~p~n", [M])
    end, ?TEST_MODULES).

setup_paths() ->
    % Add application paths
    code:add_paths(filelib:wildcard("_build/default/lib/*/ebin")),
    code:add_paths(filelib:wildcard("apps/*/ebin")),
    code:add_paths(["ebin"]).

run_all_tests(Options) ->
    io:format("Running all test suites...~n~n"),
    
    % Filter modules based on options
    Modules = case maps:get(quick, Options, false) of
        true ->
            % Skip performance tests in quick mode
            ?TEST_MODULES -- [performance_tests];
        false ->
            case maps:get(performance, Options, false) of
                true -> ?TEST_MODULES;
                false -> ?TEST_MODULES -- [performance_tests]
            end
    end,
    
    Results = lists:map(fun(Module) ->
        run_module_tests(Module, Options)
    end, Modules),
    
    % Summary
    print_summary(Results).

run_module_tests(Module, Options) ->
    io:format("~n--- Running ~p ---~n", [Module]),
    
    % Ensure module is loaded
    case code:ensure_loaded(Module) of
        {module, Module} ->
            StartTime = erlang:monotonic_time(millisecond),
            
            % Run tests
            Result = try
                case Module of
                    property_tests ->
                        % Special handling for property tests
                        run_property_tests(Module, Options);
                    _ ->
                        % Standard EUnit tests
                        run_eunit_tests(Module, Options)
                end
            catch
                Class:Reason:Stack ->
                    {error, {Class, Reason, Stack}}
            end,
            
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,
            
            {Module, Result, Duration};
        Error ->
            io:format("Error loading module ~p: ~p~n", [Module, Error]),
            {Module, {error, not_loaded}, 0}
    end.

run_eunit_tests(Module, Options) ->
    % Set EUnit options
    EunitOpts = case maps:get(verbose, Options, false) of
        true -> [verbose];
        false -> []
    end,
    
    % Run tests
    case eunit:test(Module, EunitOpts) of
        ok ->
            io:format("✓ All tests passed~n"),
            {ok, passed};
        error ->
            io:format("✗ Some tests failed~n"),
            {error, failed};
        {error, Reason} ->
            io:format("✗ Test error: ~p~n", [Reason]),
            {error, Reason}
    end.

run_property_tests(Module, Options) ->
    io:format("Running property-based tests...~n"),
    
    % Get all properties
    Props = Module:module_info(exports),
    PropFuns = [F || {F, 0} <- Props, 
                     lists:prefix("prop_", atom_to_list(F))],
    
    % Run each property
    Results = lists:map(fun(PropFun) ->
        io:format("  Testing ~p...", [PropFun]),
        case proper:quickcheck(Module:PropFun()) of
            true ->
                io:format(" ✓~n"),
                {PropFun, passed};
            false ->
                io:format(" ✗~n"),
                {PropFun, failed};
            {error, Reason} ->
                io:format(" ✗ (~p)~n", [Reason]),
                {PropFun, {error, Reason}}
        end
    end, PropFuns),
    
    % Check if all passed
    Failed = [F || {F, R} <- Results, R =/= passed],
    case Failed of
        [] -> {ok, passed};
        _ -> {error, {failed_properties, Failed}}
    end.

print_summary(Results) ->
    io:format("~n~n=== Test Summary ===~n"),
    
    TotalModules = length(Results),
    PassedModules = length([R || {_, {ok, _}, _} <- Results]),
    TotalTime = lists:sum([D || {_, _, D} <- Results]),
    
    io:format("Total modules: ~p~n", [TotalModules]),
    io:format("Passed: ~p~n", [PassedModules]),
    io:format("Failed: ~p~n", [TotalModules - PassedModules]),
    io:format("Total time: ~.2f seconds~n", [TotalTime / 1000]),
    
    % List failed modules
    Failed = [{M, R} || {M, R, _} <- Results, element(1, R) =/= ok],
    case Failed of
        [] ->
            io:format("~n✓ All test suites passed!~n"),
            halt(0);
        _ ->
            io:format("~n✗ Failed modules:~n"),
            lists:foreach(fun({M, R}) ->
                io:format("  - ~p: ~p~n", [M, R])
            end, Failed),
            halt(1)
    end.