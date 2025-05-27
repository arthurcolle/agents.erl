%%%-------------------------------------------------------------------
%%% @doc
%%% UUID Tests
%%% Tests for UUID generation
%%% @end
%%%-------------------------------------------------------------------
-module(uuid_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).
-define(UUID_V4_REGEX, "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$").

%%%===================================================================
%%% Test Generators
%%%===================================================================

uuid_test_() ->
    [
     {"Basic UUID generation", fun test_basic_generation/0},
     {"UUID format validation", fun test_format_validation/0},
     {"UUID uniqueness", fun test_uniqueness/0},
     {"UUID versions", fun test_uuid_versions/0},
     {"Binary UUID handling", fun test_binary_uuid/0},
     {"UUID parsing", fun test_uuid_parsing/0},
     {"Namespace UUIDs", fun test_namespace_uuids/0},
     {"Time-based UUIDs", fun test_time_based_uuids/0},
     {"Concurrent generation", fun test_concurrent_generation/0},
     {"Performance", fun test_performance/0},
     {"Error handling", fun test_error_handling/0},
     {"UUID comparison", fun test_uuid_comparison/0}
    ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_generation() ->
    % Generate UUID v4
    UUID1 = uuid:v4(),
    ?assert(is_binary(UUID1)),
    ?assertEqual(36, byte_size(UUID1)), % Standard UUID string length
    
    % Generate another one
    UUID2 = uuid:v4(),
    ?assertNotEqual(UUID1, UUID2),
    
    % Generate with string format
    UUIDStr = uuid:v4_string(),
    ?assert(is_list(UUIDStr)),
    ?assertEqual(36, length(UUIDStr)),
    
    % Generate multiple
    UUIDs = [uuid:v4() || _ <- lists:seq(1, 100)],
    ?assertEqual(100, length(lists:usort(UUIDs))). % All unique

test_format_validation() ->
    % Valid UUID v4
    ValidUUID = uuid:v4(),
    ?assert(uuid:is_valid(ValidUUID)),
    
    % Check v4 format specifically
    ?assertMatch({match, _}, re:run(ValidUUID, ?UUID_V4_REGEX)),
    
    % Test string format
    StringUUID = binary_to_list(ValidUUID),
    ?assert(uuid:is_valid(StringUUID)),
    
    % Invalid formats
    ?assertNot(uuid:is_valid(<<"not-a-uuid">>)),
    ?assertNot(uuid:is_valid(<<"12345678-1234-1234-1234-123456789012">>)), % Wrong version
    ?assertNot(uuid:is_valid(<<"12345678-1234-5234-1234-123456789012">>)), % v5, not v4
    ?assertNot(uuid:is_valid(<<"12345678123412341234123456789012">>)), % No hyphens
    ?assertNot(uuid:is_valid(<<>>)), % Empty
    ?assertNot(uuid:is_valid(<<"12345678-1234-4234-1234-12345678901">>)), % Too short
    ?assertNot(uuid:is_valid(<<"12345678-1234-4234-1234-1234567890123">>)), % Too long
    
    % Case insensitive
    UpperUUID = string:uppercase(binary_to_list(ValidUUID)),
    ?assert(uuid:is_valid(UpperUUID)),
    
    % Mixed case
    MixedUUID = "12345678-1234-4234-ABcd-123456789012",
    ?assert(uuid:is_valid(MixedUUID)).

test_uniqueness() ->
    % Generate many UUIDs and check uniqueness
    NumUUIDs = 10000,
    UUIDs = [uuid:v4() || _ <- lists:seq(1, NumUUIDs)],
    UniqueUUIDs = lists:usort(UUIDs),
    
    ?assertEqual(NumUUIDs, length(UniqueUUIDs)),
    
    % Check statistical properties
    % Count distribution of first byte values
    FirstBytes = [binary:first(UUID) || UUID <- UUIDs],
    Distribution = lists:foldl(fun(Byte, Acc) ->
        maps:update_with(Byte, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, FirstBytes),
    
    % Should have reasonable distribution (not all same value)
    ?assert(maps:size(Distribution) > 10),
    
    % Check no obvious patterns
    Sorted = lists:sort(UUIDs),
    Consecutive = lists:zip(lists:droplast(Sorted), tl(Sorted)),
    
    % No two consecutive UUIDs should be too similar
    lists:foreach(fun({UUID1, UUID2}) ->
        Diff = binary_diff_count(UUID1, UUID2),
        ?assert(Diff > 5) % At least 5 character differences
    end, Consecutive).

test_uuid_versions() ->
    % UUID v1 (time-based)
    UUID1 = uuid:v1(),
    ?assert(uuid:is_valid(UUID1)),
    ?assertEqual($1, binary:at(UUID1, 14)), % Version digit
    
    % Multiple v1 UUIDs should be ordered by time
    UUID1_1 = uuid:v1(),
    timer:sleep(1),
    UUID1_2 = uuid:v1(),
    timer:sleep(1),
    UUID1_3 = uuid:v1(),
    
    % Extract timestamps and verify ordering
    TS1 = uuid:get_v1_time(UUID1_1),
    TS2 = uuid:get_v1_time(UUID1_2),
    TS3 = uuid:get_v1_time(UUID1_3),
    
    ?assert(TS1 < TS2),
    ?assert(TS2 < TS3),
    
    % UUID v3 (MD5 namespace)
    Namespace = uuid:namespace_dns(),
    Name = <<"example.com">>,
    UUID3 = uuid:v3(Namespace, Name),
    ?assert(uuid:is_valid(UUID3)),
    ?assertEqual($3, binary:at(UUID3, 14)),
    
    % Same namespace and name should produce same UUID
    UUID3_2 = uuid:v3(Namespace, Name),
    ?assertEqual(UUID3, UUID3_2),
    
    % Different name should produce different UUID
    UUID3_3 = uuid:v3(Namespace, <<"example.org">>),
    ?assertNotEqual(UUID3, UUID3_3),
    
    % UUID v4 (random)
    UUID4 = uuid:v4(),
    ?assertEqual($4, binary:at(UUID4, 14)),
    
    % UUID v5 (SHA1 namespace)
    UUID5 = uuid:v5(Namespace, Name),
    ?assert(uuid:is_valid(UUID5)),
    ?assertEqual($5, binary:at(UUID5, 14)),
    
    % v5 should be deterministic too
    UUID5_2 = uuid:v5(Namespace, Name),
    ?assertEqual(UUID5, UUID5_2).

test_binary_uuid() ->
    % String to binary conversion
    StringUUID = uuid:v4_string(),
    BinaryUUID = uuid:string_to_binary(StringUUID),
    ?assertEqual(16, byte_size(BinaryUUID)), % Raw UUID is 16 bytes
    
    % Binary to string conversion
    BackToString = uuid:binary_to_string(BinaryUUID),
    ?assertEqual(string:lowercase(StringUUID), string:lowercase(BackToString)),
    
    % Direct binary generation
    RawBinary = uuid:v4_binary(),
    ?assertEqual(16, byte_size(RawBinary)),
    
    % Convert and validate
    StringFromBinary = uuid:binary_to_string(RawBinary),
    ?assert(uuid:is_valid(StringFromBinary)),
    
    % Round trip
    lists:foreach(fun(_) ->
        Original = uuid:v4(),
        Binary = uuid:string_to_binary(Original),
        Restored = uuid:binary_to_string(Binary),
        ?assertEqual(string:lowercase(binary_to_list(Original)), 
                    string:lowercase(Restored))
    end, lists:seq(1, 100)).

test_uuid_parsing() ->
    % Parse valid UUID
    ValidUUID = <<"550e8400-e29b-41d4-a716-446655440000">>,
    {ok, Parts} = uuid:parse(ValidUUID),
    
    ?assertEqual(<<"550e8400">>, maps:get(time_low, Parts)),
    ?assertEqual(<<"e29b">>, maps:get(time_mid, Parts)),
    ?assertEqual(<<"41d4">>, maps:get(time_hi_version, Parts)),
    ?assertEqual(<<"a7">>, maps:get(clock_seq_hi_variant, Parts)),
    ?assertEqual(<<"16">>, maps:get(clock_seq_low, Parts)),
    ?assertEqual(<<"446655440000">>, maps:get(node, Parts)),
    ?assertEqual(4, maps:get(version, Parts)),
    
    % Parse with different formats
    WithBraces = <<"{550e8400-e29b-41d4-a716-446655440000}">>,
    {ok, _} = uuid:parse(WithBraces),
    
    WithoutHyphens = <<"550e8400e29b41d4a716446655440000">>,
    {ok, _} = uuid:parse(WithoutHyphens),
    
    % Parse errors
    ?assertEqual({error, invalid_format}, uuid:parse(<<"invalid">>)),
    ?assertEqual({error, invalid_format}, uuid:parse(<<"550e8400-e29b-41d4-a716">>)),
    
    % Extract version and variant
    V1UUID = uuid:v1(),
    {ok, V1Parts} = uuid:parse(V1UUID),
    ?assertEqual(1, maps:get(version, V1Parts)),
    
    V4UUID = uuid:v4(),
    {ok, V4Parts} = uuid:parse(V4UUID),
    ?assertEqual(4, maps:get(version, V4Parts)).

test_namespace_uuids() ->
    % Predefined namespaces
    DNS = uuid:namespace_dns(),
    URL = uuid:namespace_url(),
    OID = uuid:namespace_oid(),
    X500 = uuid:namespace_x500(),
    
    ?assert(uuid:is_valid(DNS)),
    ?assert(uuid:is_valid(URL)),
    ?assert(uuid:is_valid(OID)),
    ?assert(uuid:is_valid(X500)),
    
    % Known values
    ?assertEqual(<<"6ba7b810-9dad-11d1-80b4-00c04fd430c8">>, DNS),
    ?assertEqual(<<"6ba7b811-9dad-11d1-80b4-00c04fd430c8">>, URL),
    ?assertEqual(<<"6ba7b812-9dad-11d1-80b4-00c04fd430c8">>, OID),
    ?assertEqual(<<"6ba7b814-9dad-11d1-80b4-00c04fd430c8">>, X500),
    
    % Custom namespace
    CustomNS = uuid:v4(),
    Name1 = <<"resource1">>,
    Name2 = <<"resource2">>,
    
    % v3 with custom namespace
    UUID3_1 = uuid:v3(CustomNS, Name1),
    UUID3_2 = uuid:v3(CustomNS, Name2),
    UUID3_1_dup = uuid:v3(CustomNS, Name1),
    
    ?assertNotEqual(UUID3_1, UUID3_2),
    ?assertEqual(UUID3_1, UUID3_1_dup),
    
    % v5 with custom namespace
    UUID5_1 = uuid:v5(CustomNS, Name1),
    UUID5_2 = uuid:v5(CustomNS, Name2),
    UUID5_1_dup = uuid:v5(CustomNS, Name1),
    
    ?assertNotEqual(UUID5_1, UUID5_2),
    ?assertEqual(UUID5_1, UUID5_1_dup),
    ?assertNotEqual(UUID3_1, UUID5_1). % v3 and v5 should differ

test_time_based_uuids() ->
    % Generate v1 UUIDs
    StartTime = erlang:system_time(nanosecond),
    
    UUIDs = lists:map(fun(N) ->
        timer:sleep(1), % Ensure different timestamps
        UUID = uuid:v1(),
        Time = uuid:get_v1_time(UUID),
        {N, UUID, Time}
    end, lists:seq(1, 10)),
    
    EndTime = erlang:system_time(nanosecond),
    
    % Verify timestamps are monotonic
    Times = [T || {_, _, T} <- UUIDs],
    SortedTimes = lists:sort(Times),
    ?assertEqual(Times, SortedTimes),
    
    % Verify timestamps are in reasonable range
    lists:foreach(fun({_, _, T}) ->
        ?assert(T >= StartTime div 100), % UUID time is in 100ns units
        ?assert(T =< EndTime div 100)
    end, UUIDs),
    
    % Test v1 with specific node
    Node = <<16#01, 16#23, 16#45, 16#67, 16#89, 16#ab>>,
    UUID_Node = uuid:v1(Node),
    {ok, Parts} = uuid:parse(UUID_Node),
    ?assertEqual(Node, uuid:string_to_binary(maps:get(node, Parts))),
    
    % Test clock sequence
    UUID1 = uuid:v1(),
    UUID2 = uuid:v1(),
    
    {ok, Parts1} = uuid:parse(UUID1),
    {ok, Parts2} = uuid:parse(UUID2),
    
    % If generated close together, should have same or incremented clock seq
    ClockSeq1 = maps:get(clock_seq_low, Parts1),
    ClockSeq2 = maps:get(clock_seq_low, Parts2),
    ?assert(ClockSeq2 >= ClockSeq1).

test_concurrent_generation() ->
    % Test thread safety of UUID generation
    NumProcesses = 100,
    UUIDsPerProcess = 100,
    
    Parent = self(),
    
    % Spawn processes to generate UUIDs concurrently
    Pids = lists:map(fun(N) ->
        spawn(fun() ->
            UUIDs = [uuid:v4() || _ <- lists:seq(1, UUIDsPerProcess)],
            Parent ! {uuid_batch, N, UUIDs}
        end)
    end, lists:seq(1, NumProcesses)),
    
    % Collect all UUIDs
    AllUUIDs = lists:flatten(
        lists:map(fun(_) ->
            receive
                {uuid_batch, _, UUIDs} -> UUIDs
            after ?TIMEOUT ->
                error(timeout)
            end
        end, Pids)
    ),
    
    % Verify total count
    ?assertEqual(NumProcesses * UUIDsPerProcess, length(AllUUIDs)),
    
    % Verify all unique
    UniqueUUIDs = lists:usort(AllUUIDs),
    ?assertEqual(length(AllUUIDs), length(UniqueUUIDs)),
    
    % Test v1 concurrency (with node IDs to avoid conflicts)
    V1Pids = lists:map(fun(N) ->
        spawn(fun() ->
            % Use process-specific node ID
            Node = <<0, 0, 0, 0, N:16>>,
            UUIDs = [uuid:v1(Node) || _ <- lists:seq(1, UUIDsPerProcess)],
            Parent ! {v1_batch, N, UUIDs}
        end)
    end, lists:seq(1, NumProcesses)),
    
    AllV1UUIDs = lists:flatten(
        lists:map(fun(_) ->
            receive
                {v1_batch, _, UUIDs} -> UUIDs
            after ?TIMEOUT ->
                error(timeout)
            end
        end, V1Pids)
    ),
    
    % All v1 UUIDs should also be unique
    UniqueV1UUIDs = lists:usort(AllV1UUIDs),
    ?assertEqual(length(AllV1UUIDs), length(UniqueV1UUIDs)).

test_performance() ->
    % Measure v4 generation speed
    NumUUIDs = 100000,
    
    {TimeV4, V4UUIDs} = timer:tc(fun() ->
        [uuid:v4() || _ <- lists:seq(1, NumUUIDs)]
    end),
    
    V4Rate = NumUUIDs * 1000000 / TimeV4,
    io:format("UUID v4 generation rate: ~.2f UUIDs/second~n", [V4Rate]),
    ?assert(V4Rate > 100000), % Should generate > 100k UUIDs/sec
    
    % Verify all unique
    ?assertEqual(NumUUIDs, length(lists:usort(V4UUIDs))),
    
    % Measure v1 generation speed
    {TimeV1, V1UUIDs} = timer:tc(fun() ->
        [uuid:v1() || _ <- lists:seq(1, NumUUIDs)]
    end),
    
    V1Rate = NumUUIDs * 1000000 / TimeV1,
    io:format("UUID v1 generation rate: ~.2f UUIDs/second~n", [V1Rate]),
    ?assert(V1Rate > 100000),
    
    % Measure parsing speed
    SampleUUID = uuid:v4(),
    {ParseTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            uuid:parse(SampleUUID)
        end, lists:seq(1, NumUUIDs))
    end),
    
    ParseRate = NumUUIDs * 1000000 / ParseTime,
    io:format("UUID parsing rate: ~.2f parses/second~n", [ParseRate]),
    ?assert(ParseRate > 500000), % Should parse > 500k/sec
    
    % Measure validation speed
    {ValidateTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            uuid:is_valid(SampleUUID)
        end, lists:seq(1, NumUUIDs))
    end),
    
    ValidateRate = NumUUIDs * 1000000 / ValidateTime,
    io:format("UUID validation rate: ~.2f validations/second~n", [ValidateRate]),
    ?assert(ValidateRate > 1000000). % Should validate > 1M/sec

test_error_handling() ->
    % Invalid input to v3/v5
    ?assertError(badarg, uuid:v3(<<"not-a-uuid">>, <<"name">>)),
    ?assertError(badarg, uuid:v5(<<"not-a-uuid">>, <<"name">>)),
    ?assertError(badarg, uuid:v3(uuid:v4(), not_binary)),
    ?assertError(badarg, uuid:v5(uuid:v4(), not_binary)),
    
    % Invalid binary conversion
    ?assertError(badarg, uuid:binary_to_string(<<"too short">>)),
    ?assertError(badarg, uuid:binary_to_string(<<"this is way too long for a uuid">>)),
    ?assertError(badarg, uuid:string_to_binary(<<"not-a-uuid">>)),
    
    % Invalid v1 node
    ?assertError(badarg, uuid:v1(<<"short">>)),
    ?assertError(badarg, uuid:v1(<<"too long node id">>)),
    ?assertError(badarg, uuid:v1(not_binary)),
    
    % Parse errors are handled gracefully
    ?assertEqual({error, invalid_format}, uuid:parse(<<>>)),
    ?assertEqual({error, invalid_format}, uuid:parse(<<"garbage">>)),
    ?assertEqual({error, invalid_format}, uuid:parse(123)),
    
    % Validation handles any input
    ?assertNot(uuid:is_valid(undefined)),
    ?assertNot(uuid:is_valid(null)),
    ?assertNot(uuid:is_valid(123)),
    ?assertNot(uuid:is_valid({uuid, 1, 2, 3})),
    ?assertNot(uuid:is_valid([1, 2, 3, 4])).

test_uuid_comparison() ->
    % Lexicographical ordering
    UUIDs = [uuid:v4() || _ <- lists:seq(1, 100)],
    Sorted = lists:sort(UUIDs),
    
    % Verify sort stability
    ?assertEqual(Sorted, lists:sort(Sorted)),
    
    % Test comparison functions
    UUID1 = <<"00000000-0000-4000-8000-000000000000">>,
    UUID2 = <<"ffffffff-ffff-4fff-bfff-ffffffffffff">>,
    
    ?assert(uuid:compare(UUID1, UUID2) < 0),
    ?assert(uuid:compare(UUID2, UUID1) > 0),
    ?assert(uuid:compare(UUID1, UUID1) =:= 0),
    
    % v1 UUIDs should be time-ordered
    V1_1 = uuid:v1(),
    timer:sleep(10),
    V1_2 = uuid:v1(),
    timer:sleep(10),
    V1_3 = uuid:v1(),
    
    % When using same node, v1 UUIDs are time-ordered
    ?assert(uuid:compare_v1_time(V1_1, V1_2) < 0),
    ?assert(uuid:compare_v1_time(V1_2, V1_3) < 0),
    ?assert(uuid:compare_v1_time(V1_1, V1_3) < 0),
    
    % Test equality
    UUID = uuid:v4(),
    UUIDString = binary_to_list(UUID),
    UUIDUpper = string:uppercase(UUIDString),
    
    ?assert(uuid:equal(UUID, UUID)),
    ?assert(uuid:equal(UUID, UUIDString)),
    ?assert(uuid:equal(UUID, UUIDUpper)),
    ?assertNot(uuid:equal(UUID, uuid:v4())).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

binary_diff_count(Bin1, Bin2) ->
    lists:sum(
        lists:zipwith(fun(B1, B2) ->
            if B1 =:= B2 -> 0; true -> 1 end
        end, binary_to_list(Bin1), binary_to_list(Bin2))
    ).