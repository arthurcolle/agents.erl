#\!/usr/bin/env escript
%%\! -pa _build/default/lib/*/ebin

main([]) ->
    io:format("Testing basic system startup...~n"),
    
    % Start applications
    ok = application:start(jsx),
    ok = application:start(openai),
    ok = application:start(agents),
    
    % Test core modules
    io:format("~nTesting core modules:~n"),
    
    % Dynamic supervisor manager
    case dynamic_supervisor_manager:start_link() of
        {ok, DsmPid} ->
            io:format("✓ Dynamic supervisor manager started: ~p~n", [DsmPid]);
        {error, {already_started, DsmPid}} ->
            io:format("✓ Dynamic supervisor manager already running: ~p~n", [DsmPid]);
        DsmErr ->
            io:format("✗ Failed to start dynamic supervisor manager: ~p~n", [DsmErr])
    end,
    
    % System introspection
    case system_introspection:start_link() of
        {ok, SiPid} ->
            io:format("✓ System introspection started: ~p~n", [SiPid]);
        {error, {already_started, SiPid}} ->
            io:format("✓ System introspection already running: ~p~n", [SiPid]);
        SiErr ->
            io:format("✗ Failed to start system introspection: ~p~n", [SiErr])
    end,
    
    % Test basic functionality
    io:format("~nTesting basic functionality:~n"),
    
    try
        {ok, State} = system_introspection:get_system_state(),
        io:format("✓ System state retrieved successfully~n"),
        io:format("  - Node: ~p~n", [maps:get(node, State)]),
        io:format("  - Processes: ~p~n", [maps:get(total_processes, State)])
    catch
        E:R:S ->
            io:format("✗ Error getting system state: ~p:~p~n~p~n", [E, R, S])
    end,
    
    io:format("~nBasic system test complete\!~n"),
    ok.
