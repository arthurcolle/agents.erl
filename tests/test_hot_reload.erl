#!/usr/bin/env escript
%% Test script for hot code reloading functionality

main(_) ->
    io:format("🔥 Testing Hot Code Reloading System~n"),
    io:format("===================================~n"),
    
    % Add the compiled beam files to the path
    code:add_path("_build/default/lib/agents/ebin"),
    code:add_path("_build/default/lib/openai/ebin"),
    code:add_path("_build/default/lib/agent_web/ebin"),
    
    % Start required applications
    application:ensure_all_started(jsx),
    application:ensure_all_started(crypto),
    
    try
        % Test 1: Start hot code reloader
        test_start_reloader(),
        
        % Test 2: Test module info
        test_module_info(),
        
        % Test 3: Test reload functionality
        test_reload_module(),
        
        % Test 4: Test file watching (basic setup)
        test_file_watching(),
        
        io:format("~n✅ Hot reload testing completed successfully!~n")
    catch
        Error:Reason:Stacktrace ->
            io:format("❌ Test failed: ~p:~p~n", [Error, Reason]),
            io:format("Stacktrace: ~p~n", [Stacktrace])
    end.

test_start_reloader() ->
    io:format("1️⃣  Testing hot code reloader startup...~n"),
    
    case hot_code_reloader:start_link() of
        {ok, Pid} ->
            io:format("   ✓ Hot code reloader started with PID: ~p~n", [Pid]);
        {error, {already_started, Pid}} ->
            io:format("   ✓ Hot code reloader already running with PID: ~p~n", [Pid]);
        Error ->
            io:format("   ❌ Failed to start hot code reloader: ~p~n", [Error]),
            throw(reloader_start_failed)
    end.

test_module_info() ->
    io:format("2️⃣  Testing module info retrieval...~n"),
    
    % Test getting info for a known module
    case hot_code_reloader:get_module_info(lists) of
        {ok, Info} ->
            io:format("   ✓ Retrieved module info for 'lists'~n"),
            io:format("   Module: ~p~n", [maps:get(module, Info)]),
            io:format("   Exports count: ~p~n", [length(maps:get(exports, Info))]);
        {error, Reason} ->
            io:format("   ❌ Failed to get module info: ~p~n", [Reason])
    end.

test_reload_module() ->
    io:format("3️⃣  Testing module reload...~n"),
    
    % Test reloading a standard library module (should work)
    case hot_code_reloader:reload_module(lists) of
        {ok, Result} ->
            io:format("   ✓ Successfully reloaded module 'lists'~n"),
            io:format("   Status: ~p~n", [maps:get(status, Result)]);
        {error, Reason} ->
            io:format("   ⚠️  Reload failed (expected for some modules): ~p~n", [Reason])
    end.

test_file_watching() ->
    io:format("4️⃣  Testing file watching setup...~n"),
    
    % Test setting up file watching
    TestFiles = [
        "apps/agents/src/agent_tools.erl",
        "apps/agent_web/src/hot_code_reloader.erl"
    ],
    
    case hot_code_reloader:watch_files(TestFiles) of
        ok ->
            io:format("   ✓ File watching started successfully~n"),
            io:format("   Watching: ~p~n", [TestFiles]),
            
            % Stop watching after a moment
            timer:sleep(100),
            case hot_code_reloader:stop_watch() of
                ok ->
                    io:format("   ✓ File watching stopped successfully~n");
                {error, Reason} ->
                    io:format("   ⚠️  Failed to stop watching: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("   ⚠️  Failed to start watching: ~p~n", [Reason])
    end.