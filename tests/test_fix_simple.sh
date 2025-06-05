#!/bin/bash

echo "🧪 Testing Streaming Token Fix"
echo "============================="
echo ""

cd /Users/agent/agents.erl

# Start a quick Erlang shell test
./rebar3 shell --eval "
io:format('✅ Testing streaming_function_handler:process_token_for_display/1~n'),
io:format('~n🔍 Test 1: Binary text~n'),
Result1 = streaming_function_handler:process_token_for_display(<<\"Hello world\">>),
io:format('   Input: <<\"Hello world\">>~n'),
io:format('   Output: ~p~n', [Result1]),

io:format('~n🔍 Test 2: String text~n'),
Result2 = streaming_function_handler:process_token_for_display(\"Sociology is\"),
io:format('   Input: \"Sociology is\"~n'),
io:format('   Output: ~p~n', [Result2]),

io:format('~n🔍 Test 3: Byte list (should be readable)~n'),
ByteList = [83, 111, 99, 105, 111, 108, 111, 103, 121],
Result3 = streaming_function_handler:process_token_for_display(ByteList),
io:format('   Input: ~p~n', [ByteList]),
io:format('   Output: ~p~n', [Result3]),

io:format('~n🔍 Test 4: Another byte list~n'),
ByteList2 = [72, 101, 108, 108, 111],
Result4 = streaming_function_handler:process_token_for_display(ByteList2),
io:format('   Input: ~p~n', [ByteList2]),
io:format('   Output: ~p~n', [Result4]),

io:format('~n✅ All tests completed!~n'),
io:format('🎯 If outputs show readable text instead of byte sequences, fix works!~n'),
halt().
" --name test_node