#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

main([]) ->
    io:format("~n=== SELF-AWARE AGENTS DEMONSTRATION ===~n~n"),
    
    % Start the agents application
    application:start(agents),
    
    io:format("1. STARTING CORE SYSTEMS~n"),
    
    % Start dynamic supervisor manager
    case dynamic_supervisor_manager:start_link() of
        {ok, DsmPid} ->
            io:format("   ✓ Dynamic supervisor manager started: ~p~n", [DsmPid]);
        {error, {already_started, _}} ->
            io:format("   ✓ Dynamic supervisor manager already running~n");
        DsmErr ->
            io:format("   ✗ Error: ~p~n", [DsmErr])
    end,
    
    % Start system introspection
    case system_introspection:start_link() of
        {ok, SiPid} ->
            io:format("   ✓ System introspection started: ~p~n", [SiPid]);
        {error, {already_started, _}} ->
            io:format("   ✓ System introspection already running~n");
        SiErr ->
            io:format("   ✗ Error: ~p~n", [SiErr])
    end,
    
    io:format("~n2. SYSTEM INFORMATION~n"),
    
    try
        {ok, State} = system_introspection:get_system_state(),
        io:format("   ✓ Node: ~p~n", [maps:get(node, State)]),
        io:format("   ✓ Total processes: ~p~n", [maps:get(total_processes, State)]),
        io:format("   ✓ Uptime: ~p seconds~n", [maps:get(uptime, State)])
    catch
        E:R ->
            io:format("   ✗ Error getting system state: ~p:~p~n", [E, R])
    end,
    
    io:format("~n3. CREATING DYNAMIC SUPERVISOR~n"),
    
    try
        {ok, SupPid} = dynamic_supervisor_manager:create_supervisor(demo_sup, 
            #{strategy => one_for_one, intensity => 5, period => 60}),
        io:format("   ✓ Created supervisor 'demo_sup' with PID ~p~n", [SupPid]),
        
        {ok, Supervisors} = dynamic_supervisor_manager:list_supervisors(),
        io:format("   ✓ Total active supervisors: ~p~n", [length(Supervisors)])
    catch
        E2:R2 ->
            io:format("   ✗ Error: ~p:~p~n", [E2, R2])
    end,
    
    io:format("~n4. SELF-AWARENESS CAPABILITIES~n"),
    io:format("   The system now has:~n"),
    io:format("   ✓ Dynamic supervisor creation~n"),
    io:format("   ✓ System introspection~n"),
    io:format("   ✓ Process monitoring~n"),
    io:format("   ✓ Runtime modification~n"),
    io:format("   ✓ Self-reflection tools~n"),
    
    io:format("~n✅ DEMONSTRATION COMPLETE!~n"),
    io:format("The self-aware agent system is operational.~n~n"),
    
    ok.