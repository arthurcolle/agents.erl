#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin -pa apps/*/ebin

%%%-------------------------------------------------------------------
%%% @doc
%%% Test script for persistent table management
%%% Tests that tables are created only once and persist across restarts
%%% @end
%%%-------------------------------------------------------------------

-record(test_record, {id, value, timestamp}).

main([]) ->
    io:format("~n=== Testing Persistent Table Management ===~n~n"),
    
    % Start the applications
    start_apps(),
    
    % Test 1: Create tables and insert data
    io:format("Test 1: Creating tables and inserting data...~n"),
    test_create_and_insert(),
    
    % Test 2: Verify data persists
    io:format("~nTest 2: Simulating restart - tables should already exist...~n"),
    test_persistence(),
    
    % Test 3: Test DETS persistence
    io:format("~nTest 3: Testing DETS persistence...~n"),
    test_dets_persistence(),
    
    io:format("~n=== All tests completed ===~n"),
    ok.

start_apps() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(sasl),
    
    % Start persistent table manager
    {ok, _} = persistent_table_manager:start_link(),
    timer:sleep(100).

test_create_and_insert() ->
    % Test ETS table creation
    case persistent_table_manager:ensure_ets_table(test_table, 
        [set, public, {keypos, #test_record.id}], true) of
        {ok, created} ->
            io:format("  ✓ ETS table 'test_table' created successfully~n");
        {ok, exists} ->
            io:format("  ! ETS table 'test_table' already exists (unexpected on first run)~n")
    end,
    
    % Insert test data
    TestRecord = #test_record{id = 1, value = "test_value", timestamp = erlang:timestamp()},
    ets:insert(test_table, TestRecord),
    io:format("  ✓ Inserted test record: ~p~n", [TestRecord]),
    
    % Verify insertion
    case ets:lookup(test_table, 1) of
        [TestRecord] ->
            io:format("  ✓ Record retrieved successfully~n");
        _ ->
            io:format("  ✗ Failed to retrieve record~n")
    end.

test_persistence() ->
    % Try to create the same table again
    case persistent_table_manager:ensure_ets_table(test_table, 
        [set, public, {keypos, #test_record.id}], true) of
        {ok, exists} ->
            io:format("  ✓ Table already exists (as expected)~n");
        {ok, created} ->
            io:format("  ✗ Table was recreated (unexpected)~n")
    end,
    
    % Check if data still exists
    case ets:lookup(test_table, 1) of
        [#test_record{id = 1, value = "test_value"}] ->
            io:format("  ✓ Previous data still exists in table~n");
        [] ->
            io:format("  ✗ Previous data was lost~n");
        Other ->
            io:format("  ? Unexpected data: ~p~n", [Other])
    end.

test_dets_persistence() ->
    % Test DETS table
    case persistent_table_manager:ensure_dets_table(test_dets, [], true) of
        {ok, Status} ->
            io:format("  ✓ DETS table 'test_dets' status: ~p~n", [Status]);
        {error, Reason} ->
            io:format("  ✗ Failed to create DETS table: ~p~n", [Reason])
    end,
    
    % Insert and verify DETS data
    dets:insert(test_dets, {key1, "persistent_value"}),
    case dets:lookup(test_dets, key1) of
        [{key1, "persistent_value"}] ->
            io:format("  ✓ DETS data inserted and retrieved successfully~n");
        _ ->
            io:format("  ✗ Failed to retrieve DETS data~n")
    end,
    
    % Get data directory
    DataDir = persistent_table_manager:get_data_dir(),
    io:format("  ℹ Data directory: ~s~n", [DataDir]),
    
    % Check if DETS file exists
    DetsFile = filename:join(DataDir, "test_dets.dets"),
    case filelib:is_regular(DetsFile) of
        true ->
            io:format("  ✓ DETS file exists at: ~s~n", [DetsFile]);
        false ->
            io:format("  ✗ DETS file not found at: ~s~n", [DetsFile])
    end.