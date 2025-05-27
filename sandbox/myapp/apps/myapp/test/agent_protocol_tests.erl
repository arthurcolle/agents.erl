%%%-------------------------------------------------------------------
%%% @doc
%%% Agent Protocol Tests
%%% Tests for message encoding/decoding
%%% @end
%%%-------------------------------------------------------------------
-module(agent_protocol_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 5000).

%%%===================================================================
%%% Test Generators
%%%===================================================================

agent_protocol_test_() ->
    [
     {"Basic encoding/decoding", fun test_basic_encoding/0},
     {"Message types", fun test_message_types/0},
     {"Complex data structures", fun test_complex_structures/0},
     {"Binary data handling", fun test_binary_data/0},
     {"Protocol versioning", fun test_versioning/0},
     {"Error messages", fun test_error_messages/0},
     {"Message validation", fun test_message_validation/0},
     {"Compression", fun test_compression/0},
     {"Encryption", fun test_encryption/0},
     {"Performance", fun test_performance/0},
     {"Edge cases", fun test_edge_cases/0},
     {"Backward compatibility", fun test_backward_compatibility/0}
    ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_basic_encoding() ->
    % Simple message
    Message = #{
        id => <<"msg_123">>,
        type => <<"request">>,
        from => <<"agent_1">>,
        to => <<"agent_2">>,
        payload => #{data => <<"Hello">>}
    },
    
    % Encode
    Encoded = agent_protocol:encode(Message),
    ?assert(is_binary(Encoded)),
    
    % Decode
    {ok, Decoded} = agent_protocol:decode(Encoded),
    ?assertEqual(Message, Decoded),
    
    % Round-trip test
    lists:foreach(fun(_) ->
        TestMsg = generate_random_message(),
        Enc = agent_protocol:encode(TestMsg),
        {ok, Dec} = agent_protocol:decode(Enc),
        ?assertEqual(TestMsg, Dec)
    end, lists:seq(1, 100)).

test_message_types() ->
    % Test all standard message types
    MessageTypes = [
        {request, #{
            id => <<"req_1">>,
            type => <<"request">>,
            method => <<"get_status">>,
            params => #{}
        }},
        {response, #{
            id => <<"resp_1">>,
            type => <<"response">>,
            request_id => <<"req_1">>,
            result => #{status => <<"ok">>}
        }},
        {notification, #{
            id => <<"notif_1">>,
            type => <<"notification">>,
            event => <<"status_changed">>,
            data => #{new_status => <<"active">>}
        }},
        {error, #{
            id => <<"err_1">>,
            type => <<"error">>,
            code => 404,
            message => <<"Not found">>,
            details => #{resource => <<"agent_99">>}
        }},
        {heartbeat, #{
            id => <<"hb_1">>,
            type => <<"heartbeat">>,
            timestamp => erlang:system_time(millisecond)
        }},
        {broadcast, #{
            id => <<"bc_1">>,
            type => <<"broadcast">>,
            topic => <<"system">>,
            message => <<"System maintenance at midnight">>
        }}
    ],
    
    lists:foreach(fun({Type, Msg}) ->
        Encoded = agent_protocol:encode(Msg),
        {ok, Decoded} = agent_protocol:decode(Encoded),
        ?assertEqual(Msg, Decoded),
        ?assertEqual(maps:get(type, Decoded), atom_to_binary(Type, utf8))
    end, MessageTypes).

test_complex_structures() ->
    % Nested maps
    NestedMsg = #{
        id => <<"nested_1">>,
        type => <<"data">>,
        payload => #{
            user => #{
                id => 123,
                name => <<"John Doe">>,
                preferences => #{
                    theme => <<"dark">>,
                    language => <<"en">>,
                    notifications => #{
                        email => true,
                        push => false,
                        sms => true
                    }
                },
                tags => [<<"admin">>, <<"beta_tester">>, <<"premium">>]
            },
            metadata => #{
                created_at => {{2024, 1, 15}, {10, 30, 0}},
                updated_at => {{2024, 1, 15}, {14, 45, 30}},
                version => 3
            }
        }
    },
    
    Encoded = agent_protocol:encode(NestedMsg),
    {ok, Decoded} = agent_protocol:decode(Encoded),
    ?assertEqual(NestedMsg, Decoded),
    
    % Lists of various types
    ListMsg = #{
        id => <<"list_1">>,
        type => <<"data">>,
        arrays => #{
            numbers => [1, 2, 3, 4, 5],
            strings => [<<"a">>, <<"b">>, <<"c">>],
            mixed => [1, <<"two">>, 3.14, true, null],
            empty => [],
            nested => [[1, 2], [3, 4], [5, 6]]
        }
    },
    
    EncodedList = agent_protocol:encode(ListMsg),
    {ok, DecodedList} = agent_protocol:decode(EncodedList),
    ?assertEqual(ListMsg, DecodedList).

test_binary_data() ->
    % Raw binary data
    BinaryData = crypto:strong_rand_bytes(1024),
    
    BinaryMsg = #{
        id => <<"bin_1">>,
        type => <<"binary">>,
        data => BinaryData,
        checksum => crypto:hash(sha256, BinaryData)
    },
    
    Encoded = agent_protocol:encode(BinaryMsg),
    {ok, Decoded} = agent_protocol:decode(Encoded),
    
    ?assertEqual(BinaryData, maps:get(data, Decoded)),
    ?assertEqual(maps:get(checksum, BinaryMsg), maps:get(checksum, Decoded)),
    
    % Large binary
    LargeBinary = crypto:strong_rand_bytes(1024 * 1024), % 1MB
    LargeMsg = #{
        id => <<"large_1">>,
        type => <<"file">>,
        filename => <<"data.bin">>,
        content => LargeBinary,
        size => byte_size(LargeBinary)
    },
    
    EncodedLarge = agent_protocol:encode(LargeMsg),
    {ok, DecodedLarge} = agent_protocol:decode(EncodedLarge),
    
    ?assertEqual(LargeBinary, maps:get(content, DecodedLarge)),
    ?assertEqual(byte_size(LargeBinary), maps:get(size, DecodedLarge)).

test_versioning() ->
    % Version 1 message
    V1Msg = #{
        version => 1,
        id => <<"v1_msg">>,
        type => <<"request">>,
        data => <<"simple data">>
    },
    
    % Version 2 message with additional fields
    V2Msg = #{
        version => 2,
        id => <<"v2_msg">>,
        type => <<"request">>,
        data => <<"enhanced data">>,
        metadata => #{source => <<"agent">>, priority => high},
        timestamp => erlang:system_time(millisecond)
    },
    
    % Encode with version
    V1Encoded = agent_protocol:encode(V1Msg, #{version => 1}),
    V2Encoded = agent_protocol:encode(V2Msg, #{version => 2}),
    
    % Decode and verify versions
    {ok, V1Decoded} = agent_protocol:decode(V1Encoded),
    {ok, V2Decoded} = agent_protocol:decode(V2Encoded),
    
    ?assertEqual(1, maps:get(version, V1Decoded)),
    ?assertEqual(2, maps:get(version, V2Decoded)),
    
    % Test version negotiation
    ?assertEqual({ok, 2}, agent_protocol:negotiate_version([1, 2, 3], [2, 3, 4])),
    ?assertEqual({error, no_common_version}, 
                 agent_protocol:negotiate_version([1, 2], [3, 4])).

test_error_messages() ->
    % Standard error codes
    ErrorCodes = [
        {400, <<"Bad Request">>, <<"Invalid parameters">>},
        {401, <<"Unauthorized">>, <<"Authentication required">>},
        {403, <<"Forbidden">>, <<"Access denied">>},
        {404, <<"Not Found">>, <<"Resource not found">>},
        {500, <<"Internal Server Error">>, <<"Something went wrong">>},
        {503, <<"Service Unavailable">>, <<"Service temporarily unavailable">>}
    ],
    
    lists:foreach(fun({Code, Status, Detail}) ->
        ErrorMsg = agent_protocol:create_error_message(Code, Status, Detail),
        
        ?assertEqual(<<"error">>, maps:get(type, ErrorMsg)),
        ?assertEqual(Code, maps:get(code, ErrorMsg)),
        ?assertEqual(Status, maps:get(message, ErrorMsg)),
        ?assertEqual(Detail, maps:get(details, ErrorMsg)),
        
        % Encode/decode error
        Encoded = agent_protocol:encode(ErrorMsg),
        {ok, Decoded} = agent_protocol:decode(Encoded),
        ?assertEqual(ErrorMsg, Decoded)
    end, ErrorCodes),
    
    % Custom error with stack trace
    StackTrace = [
        {module1, function1, 2, [{file, "module1.erl"}, {line, 42}]},
        {module2, function2, 3, [{file, "module2.erl"}, {line, 100}]}
    ],
    
    DetailedError = agent_protocol:create_error_message(500, <<"Crash">>, 
        #{reason => <<"division_by_zero">>, stack => StackTrace}),
    
    EncodedError = agent_protocol:encode(DetailedError),
    {ok, DecodedError} = agent_protocol:decode(EncodedError),
    
    Details = maps:get(details, DecodedError),
    ?assertEqual(<<"division_by_zero">>, maps:get(reason, Details)).

test_message_validation() ->
    % Valid message
    ValidMsg = #{
        id => <<"valid_1">>,
        type => <<"request">>,
        from => <<"agent_a">>,
        to => <<"agent_b">>,
        payload => #{action => <<"ping">>}
    },
    
    ?assertEqual(ok, agent_protocol:validate_message(ValidMsg)),
    
    % Missing required fields
    ?assertEqual({error, {missing_field, id}}, 
                 agent_protocol:validate_message(maps:remove(id, ValidMsg))),
    ?assertEqual({error, {missing_field, type}},
                 agent_protocol:validate_message(maps:remove(type, ValidMsg))),
    
    % Invalid field types
    ?assertEqual({error, {invalid_type, id, binary}},
                 agent_protocol:validate_message(ValidMsg#{id => 123})),
    ?assertEqual({error, {invalid_type, type, binary}},
                 agent_protocol:validate_message(ValidMsg#{type => request})),
    
    % Invalid message type
    ?assertEqual({error, {invalid_message_type, <<"unknown">>}},
                 agent_protocol:validate_message(ValidMsg#{type => <<"unknown">>})),
    
    % Schema validation for specific types
    RequestMsg = #{
        id => <<"req_1">>,
        type => <<"request">>,
        method => <<"call">>,
        params => [1, 2, 3]
    },
    
    ?assertEqual(ok, agent_protocol:validate_message(RequestMsg)),
    
    % Missing method in request
    ?assertEqual({error, {missing_field, method}},
                 agent_protocol:validate_message(maps:remove(method, RequestMsg))).

test_compression() ->
    % Test different compression levels
    LargeData = list_to_binary(lists:duplicate(1000, "This is a test string. ")),
    
    Message = #{
        id => <<"compress_1">>,
        type => <<"data">>,
        payload => LargeData
    },
    
    % No compression
    UncompressedSize = byte_size(agent_protocol:encode(Message)),
    
    % With compression
    CompressedMsg = agent_protocol:encode(Message, #{compress => true}),
    CompressedSize = byte_size(CompressedMsg),
    
    ?assert(CompressedSize < UncompressedSize),
    io:format("Compression ratio: ~.2f%~n", 
              [(UncompressedSize - CompressedSize) / UncompressedSize * 100]),
    
    % Decode compressed
    {ok, Decompressed} = agent_protocol:decode(CompressedMsg),
    ?assertEqual(LargeData, maps:get(payload, Decompressed)),
    
    % Test compression levels
    Levels = [1, 6, 9], % Min, default, max
    Sizes = lists:map(fun(Level) ->
        Compressed = agent_protocol:encode(Message, #{compress => true, 
                                                     compress_level => Level}),
        byte_size(Compressed)
    end, Levels),
    
    % Higher compression level should result in smaller size
    ?assert(lists:nth(3, Sizes) =< lists:nth(1, Sizes)).

test_encryption() ->
    % Generate encryption key
    Key = crypto:strong_rand_bytes(32),
    
    SensitiveMsg = #{
        id => <<"secret_1">>,
        type => <<"confidential">>,
        payload => #{
            password => <<"super_secret_password">>,
            api_key => <<"sk_live_abc123xyz">>,
            ssn => <<"123-45-6789">>
        }
    },
    
    % Encode with encryption
    Encrypted = agent_protocol:encode(SensitiveMsg, #{encrypt => true, key => Key}),
    
    % Verify it's actually encrypted (shouldn't contain plaintext)
    ?assertEqual(nomatch, binary:match(Encrypted, <<"super_secret_password">>)),
    ?assertEqual(nomatch, binary:match(Encrypted, <<"sk_live_abc123xyz">>)),
    
    % Decode with correct key
    {ok, Decrypted} = agent_protocol:decode(Encrypted, #{key => Key}),
    ?assertEqual(SensitiveMsg, Decrypted),
    
    % Try to decode with wrong key
    WrongKey = crypto:strong_rand_bytes(32),
    Result = agent_protocol:decode(Encrypted, #{key => WrongKey}),
    ?assertMatch({error, _}, Result),
    
    % Test authenticated encryption (AEAD)
    AAD = <<"additional authenticated data">>,
    EncryptedAAD = agent_protocol:encode(SensitiveMsg, 
        #{encrypt => true, key => Key, aad => AAD}),
    
    % Decode with correct AAD
    {ok, _} = agent_protocol:decode(EncryptedAAD, #{key => Key, aad => AAD}),
    
    % Fail with wrong AAD
    WrongAAD = <<"wrong authenticated data">>,
    ?assertMatch({error, _}, 
                 agent_protocol:decode(EncryptedAAD, #{key => Key, aad => WrongAAD})).

test_performance() ->
    % Simple message performance
    SimpleMsg = #{
        id => <<"perf_1">>,
        type => <<"test">>,
        data => <<"Hello, World!">>
    },
    
    NumIterations = 10000,
    
    % Encoding performance
    {EncodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            agent_protocol:encode(SimpleMsg)
        end, lists:seq(1, NumIterations))
    end),
    
    EncodeRate = NumIterations * 1000000 / EncodeTime,
    io:format("Encoding rate: ~.2f messages/second~n", [EncodeRate]),
    ?assert(EncodeRate > 10000), % Should encode > 10k msgs/sec
    
    % Decoding performance
    Encoded = agent_protocol:encode(SimpleMsg),
    {DecodeTime, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            agent_protocol:decode(Encoded)
        end, lists:seq(1, NumIterations))
    end),
    
    DecodeRate = NumIterations * 1000000 / DecodeTime,
    io:format("Decoding rate: ~.2f messages/second~n", [DecodeRate]),
    ?assert(DecodeRate > 10000), % Should decode > 10k msgs/sec
    
    % Large message performance
    LargeMsg = #{
        id => <<"large_perf">>,
        type => <<"data">>,
        payload => lists:foldl(fun(N, Acc) ->
            Acc#{integer_to_binary(N) => generate_random_data()}
        end, #{}, lists:seq(1, 100))
    },
    
    {LargeEncTime, LargeEncoded} = timer:tc(fun() ->
        agent_protocol:encode(LargeMsg)
    end),
    
    {LargeDecTime, _} = timer:tc(fun() ->
        agent_protocol:decode(LargeEncoded)
    end),
    
    io:format("Large message (~.2f KB): Encode ~p μs, Decode ~p μs~n",
              [byte_size(LargeEncoded) / 1024, LargeEncTime, LargeDecTime]).

test_edge_cases() ->
    % Empty message
    EmptyMsg = #{},
    ?assertMatch({error, _}, agent_protocol:validate_message(EmptyMsg)),
    
    % Unicode handling
    UnicodeMsg = #{
        id => <<"unicode_1">>,
        type => <<"text">>,
        content => <<"Hello 世界 🌍 Здравствуй мир"/utf8>>
    },
    
    UnicodeEncoded = agent_protocol:encode(UnicodeMsg),
    {ok, UnicodeDecoded} = agent_protocol:decode(UnicodeEncoded),
    ?assertEqual(maps:get(content, UnicodeMsg), maps:get(content, UnicodeDecoded)),
    
    % Special values
    SpecialMsg = #{
        id => <<"special_1">>,
        type => <<"data">>,
        values => #{
            null => null,
            true_val => true,
            false_val => false,
            zero => 0,
            negative => -42,
            float => 3.14159,
            infinity => infinity,
            neg_infinity => neg_infinity,
            nan => nan
        }
    },
    
    SpecialEncoded = agent_protocol:encode(SpecialMsg),
    {ok, SpecialDecoded} = agent_protocol:decode(SpecialEncoded),
    
    Values = maps:get(values, SpecialDecoded),
    ?assertEqual(null, maps:get(null, Values)),
    ?assertEqual(true, maps:get(true_val, Values)),
    ?assertEqual(false, maps:get(false_val, Values)),
    ?assertEqual(0, maps:get(zero, Values)),
    ?assertEqual(-42, maps:get(negative, Values)),
    
    % Very long strings
    LongString = list_to_binary(lists:duplicate(10000, $a)),
    LongMsg = #{
        id => <<"long_1">>,
        type => <<"text">>,
        content => LongString
    },
    
    LongEncoded = agent_protocol:encode(LongMsg),
    {ok, LongDecoded} = agent_protocol:decode(LongEncoded),
    ?assertEqual(LongString, maps:get(content, LongDecoded)),
    
    % Circular reference prevention
    CircularMsg = #{id => <<"circ_1">>, type => <<"data">>},
    % Would need special handling to prevent infinite recursion
    
    % Invalid binary data
    InvalidBinary = <<0, 1, 2, 255, 254, 253>>,
    ?assertMatch({error, _}, agent_protocol:decode(InvalidBinary)).

test_backward_compatibility() ->
    % Old format message (v1)
    OldFormatBinary = <<"{\"id\":\"old_1\",\"type\":\"request\",\"data\":\"test\"}">>,
    
    % Should still decode old format
    {ok, Decoded} = agent_protocol:decode(OldFormatBinary, #{legacy => true}),
    ?assertEqual(<<"old_1">>, maps:get(id, Decoded)),
    ?assertEqual(<<"request">>, maps:get(type, Decoded)),
    
    % New format should be readable by old decoders
    NewMsg = #{
        id => <<"new_1">>,
        type => <<"request">>,
        version => 2,
        data => <<"test">>,
        new_field => <<"ignored by old decoder">>
    },
    
    Encoded = agent_protocol:encode(NewMsg),
    {ok, DecodedByOld} = agent_protocol:decode(Encoded, #{version => 1}),
    
    % Old decoder should ignore unknown fields
    ?assertEqual(<<"new_1">>, maps:get(id, DecodedByOld)),
    ?assertEqual(<<"test">>, maps:get(data, DecodedByOld)),
    ?assertNot(maps:is_key(new_field, DecodedByOld)).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

generate_random_message() ->
    Types = [<<"request">>, <<"response">>, <<"notification">>],
    #{
        id => generate_id(),
        type => lists:nth(rand:uniform(length(Types)), Types),
        timestamp => erlang:system_time(millisecond),
        payload => generate_random_data()
    }.

generate_id() ->
    list_to_binary("msg_" ++ integer_to_list(rand:uniform(1000000))).

generate_random_data() ->
    case rand:uniform(5) of
        1 -> rand:uniform(1000);
        2 -> list_to_binary(generate_random_string(rand:uniform(50)));
        3 -> lists:map(fun(_) -> rand:uniform(100) end, lists:seq(1, rand:uniform(10)));
        4 -> #{key => generate_random_string(10), value => rand:uniform(100)};
        5 -> rand:uniform() < 0.5
    end.

generate_random_string(Length) ->
    lists:map(fun(_) -> 
        rand:uniform(26) + 96  % lowercase letters
    end, lists:seq(1, Length)).