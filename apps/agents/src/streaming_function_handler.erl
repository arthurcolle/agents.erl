%% streaming_function_handler.erl
%% Handles streaming function calls with proper aggregation and execution
%% Uses jiffy for JSON serialization of streaming tokens
-module(streaming_function_handler).

-export([
    init_accumulator/0,
    process_stream_event/3,
    finalize_stream/3,
    handle_responses_api_stream/3,
    handle_chat_api_stream/3,
    process_token_for_display/1,
    serialize_streaming_token/1,
    serialize_streaming_response/1
]).

%% Logging macros with fallback
-define(LOG_INFO(Msg), log_safe(info, Msg)).
-define(LOG_INFO(Msg, Args), log_safe(info, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), log_safe(error, Msg)).
-define(LOG_ERROR(Msg, Args), log_safe(error, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), log_safe(warning, Msg)).
-define(LOG_WARNING(Msg, Args), log_safe(warning, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), log_safe(debug, Msg)).
-define(LOG_DEBUG(Msg, Args), log_safe(debug, io_lib:format(Msg, Args))).

-record(accumulator, {
    content = <<>> :: binary(),
    tool_calls = [] :: list(),
    tool_call_map = #{} :: map(),
    metadata = #{} :: map()
}).

%% Initialize accumulator for streaming
init_accumulator() ->
    #accumulator{}.

%% Process a single stream event and update accumulator
process_stream_event(Event, Accumulator, StreamPid) ->
    EventType = parse_event_type(Event),
    ?LOG_DEBUG("[STREAM_FUNC] Processing event type: ~p", [element(1, EventType)]),
    case EventType of
        {content_delta, Delta} ->
            NewContent = <<(Accumulator#accumulator.content)/binary, Delta/binary>>,
            ProcessedDelta = process_token_for_display(Delta),
            ?LOG_INFO("[STREAM_FUNC] 🔤 Sending token to ~p: ~p -> ~p", [StreamPid, Delta, ProcessedDelta]),
            StreamPid ! {stream_token, ProcessedDelta},
            {continue, Accumulator#accumulator{content = NewContent}};
            
        {tool_call_start, ToolCall} ->
            Index = maps:get(<<"index">>, ToolCall, 0),
            ?LOG_INFO("[STREAM_FUNC] Tool call started - Index: ~p, Call: ~p", [Index, ToolCall]),
            CallMap = Accumulator#accumulator.tool_call_map,
            NewCallMap = CallMap#{Index => initialize_tool_call(ToolCall)},
            StreamPid ! {stream_function_call_started, ToolCall},
            {continue, Accumulator#accumulator{tool_call_map = NewCallMap}};
            
        {tool_call_delta, Index, Delta} ->
            ?LOG_DEBUG("[STREAM_FUNC] Tool call delta - Index: ~p", [Index]),
            CallMap = Accumulator#accumulator.tool_call_map,
            case maps:get(Index, CallMap, undefined) of
                undefined ->
                    ?LOG_WARNING("[STREAM_FUNC] Delta for unknown tool call index ~p, initializing", [Index]),
                    % Start a new tool call if we haven't seen this index
                    NewCallMap = CallMap#{Index => initialize_tool_call(Delta)},
                    {continue, Accumulator#accumulator{tool_call_map = NewCallMap}};
                ExistingCall ->
                    UpdatedCall = merge_tool_call_delta(ExistingCall, Delta),
                    NewCallMap = CallMap#{Index => UpdatedCall},
                    StreamPid ! {stream_function_arguments_delta, Index, Delta},
                    {continue, Accumulator#accumulator{tool_call_map = NewCallMap}}
            end;
            
        {tool_call_complete, Index} ->
            CallMap = Accumulator#accumulator.tool_call_map,
            case maps:get(Index, CallMap, undefined) of
                undefined ->
                    {continue, Accumulator};
                CompletedCall ->
                    % Move from map to list when complete
                    NewToolCalls = [CompletedCall | Accumulator#accumulator.tool_calls],
                    NewCallMap = maps:remove(Index, CallMap),
                    StreamPid ! {stream_function_call_complete, CompletedCall},
                    {continue, Accumulator#accumulator{
                        tool_calls = NewToolCalls,
                        tool_call_map = NewCallMap
                    }}
            end;
            
        {stream_done, Metadata} ->
            % Finalize any remaining tool calls
            RemainingCalls = maps:values(Accumulator#accumulator.tool_call_map),
            AllToolCalls = lists:reverse(Accumulator#accumulator.tool_calls) ++ RemainingCalls,
            FinalAccumulator = Accumulator#accumulator{
                tool_calls = AllToolCalls,
                tool_call_map = #{},
                metadata = Metadata
            },
            {done, FinalAccumulator};
            
        {error, Reason} ->
            StreamPid ! {stream_error, Reason},
            {error, Reason};
            
        unknown ->
            {continue, Accumulator}
    end.

%% Parse event type based on API format
parse_event_type(Event) when is_map(Event) ->
    case maps:get(<<"type">>, Event, undefined) of
        %% Responses API event types
        <<"response.output_text.delta">> ->
            Delta = maps:get(<<"delta">>, Event, <<"">>),
            {content_delta, Delta};
            
        <<"response.output_item.added">> ->
            Item = maps:get(<<"item">>, Event, #{}),
            case maps:get(<<"type">>, Item, undefined) of
                <<"function_call">> ->
                    Index = maps:get(<<"index">>, Event, 0),
                    {tool_call_start, Item#{<<"index">> => Index}};
                _ ->
                    unknown
            end;
            
        <<"response.function_call_arguments.delta">> ->
            Index = maps:get(<<"item_index">>, Event, 0),
            Delta = maps:get(<<"delta">>, Event, <<"">>),
            {tool_call_delta, Index, #{<<"arguments">> => Delta}};
            
        <<"response.function_call_arguments.done">> ->
            Index = maps:get(<<"item_index">>, Event, 0),
            {tool_call_complete, Index};
            
        <<"response.done">> ->
            Response = maps:get(<<"response">>, Event, #{}),
            {stream_done, Response};
            
        <<"response.failed">> ->
            Error = maps:get(<<"error">>, Event, <<"Unknown error">>),
            {error, Error};
            
        %% Chat API streaming (via processed events)
        undefined ->
            case maps:get(type, Event, undefined) of
                content ->
                    {content_delta, maps:get(data, Event, <<>>)};
                tool_calls ->
                    ToolCalls = maps:get(data, Event, []),
                    process_chat_api_tool_calls(ToolCalls);
                _ ->
                    parse_chat_api_event(Event)
            end;
            
        _ ->
            unknown
    end.

%% Parse Chat API streaming event
parse_chat_api_event(Event) ->
    case maps:get(<<"choices">>, Event, []) of
        [Choice | _] ->
            Delta = maps:get(<<"delta">>, Choice, #{}),
            case Delta of
                #{<<"content">> := Content} when Content =/= null ->
                    {content_delta, Content};
                #{<<"tool_calls">> := ToolCalls} when ToolCalls =/= null ->
                    process_chat_api_tool_calls(ToolCalls);
                _ ->
                    unknown
            end;
        _ ->
            unknown
    end.

%% Process tool calls from Chat API format
process_chat_api_tool_calls([]) ->
    unknown;
process_chat_api_tool_calls([ToolCall | _]) ->
    Index = maps:get(<<"index">>, ToolCall, 0),
    case maps:get(<<"id">>, ToolCall, undefined) of
        undefined ->
            % This is a delta update
            {tool_call_delta, Index, ToolCall};
        _Id ->
            % This is a new tool call
            {tool_call_start, ToolCall}
    end.

%% Initialize a tool call structure
initialize_tool_call(ToolCall) ->
    #{
        <<"id">> => maps:get(<<"id">>, ToolCall, generate_call_id()),
        <<"type">> => maps:get(<<"type">>, ToolCall, <<"function">>),
        <<"function">> => #{
            <<"name">> => get_function_name(ToolCall),
            <<"arguments">> => get_function_arguments(ToolCall)
        }
    }.

%% Merge tool call delta into existing call
merge_tool_call_delta(ExistingCall, Delta) ->
    ExistingFunc = maps:get(<<"function">>, ExistingCall, #{}),
    
    % Update function name if provided
    NewName = case get_function_name(Delta) of
        <<"">> -> maps:get(<<"name">>, ExistingFunc, <<"">>);
        Name -> Name
    end,
    
    % Accumulate arguments
    ExistingArgs = maps:get(<<"arguments">>, ExistingFunc, <<"">>),
    DeltaArgs = get_function_arguments(Delta),
    NewArgs = <<ExistingArgs/binary, DeltaArgs/binary>>,
    
    % Update the call
    ExistingCall#{
        <<"function">> => ExistingFunc#{
            <<"name">> => NewName,
            <<"arguments">> => NewArgs
        }
    }.

%% Extract function name from various formats
get_function_name(Data) ->
    case maps:get(<<"function">>, Data, undefined) of
        undefined ->
            maps:get(<<"name">>, Data, <<"">>);
        FuncData ->
            maps:get(<<"name">>, FuncData, <<"">>)
    end.

%% Extract function arguments from various formats
get_function_arguments(Data) ->
    case maps:get(<<"function">>, Data, undefined) of
        undefined ->
            maps:get(<<"arguments">>, Data, <<"">>);
        FuncData ->
            maps:get(<<"arguments">>, FuncData, <<"">>)
    end.

%% Generate a unique call ID
generate_call_id() ->
    <<"call_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

%% Finalize the stream and execute any tool calls
finalize_stream(Accumulator, StreamPid, State) ->
    Content = Accumulator#accumulator.content,
    ToolCalls = Accumulator#accumulator.tool_calls,
    
    ?LOG_INFO("[STREAM_FUNC] Finalizing stream - Content length: ~p, Tool calls: ~p", 
              [byte_size(Content), length(ToolCalls)]),
    
    case ToolCalls of
        [] ->
            ?LOG_INFO("[STREAM_FUNC] No tool calls detected, returning content only"),
            % No tool calls, just content
            Response = #{
                message => Content,
                tool_calls => [],
                metadata => Accumulator#accumulator.metadata
            },
            StreamPid ! {stream_complete, Response},
            {ok, Response, State};
            
        _ ->
            ?LOG_INFO("[STREAM_FUNC] Detected ~p tool calls, executing...", [length(ToolCalls)]),
            ?LOG_DEBUG("[STREAM_FUNC] Tool calls: ~p", [ToolCalls]),
            % Execute tool calls and stream results
            StreamPid ! {stream_function_execution_start},
            execute_and_stream_tool_results(ToolCalls, Content, StreamPid, State)
    end.

%% Execute tool calls and stream the results
execute_and_stream_tool_results(ToolCalls, InitialContent, StreamPid, State) ->
    ?LOG_INFO("[STREAM_FUNC] Starting parallel tool execution for ~p tools", [length(ToolCalls)]),
    % Execute all tool calls
    ToolResults = execute_tool_calls_parallel(ToolCalls),
    
    ?LOG_INFO("[STREAM_FUNC] Tool execution complete, streaming results"),
    % Stream tool execution results
    lists:foreach(fun({ToolCall, Result}) ->
        ?LOG_DEBUG("[STREAM_FUNC] Streaming result for tool: ~p", 
                   [maps:get(<<"name">>, maps:get(<<"function">>, ToolCall, #{}), unknown)]),
        StreamPid ! {stream_tool_result, #{
            tool_call => ToolCall,
            result => Result
        }}
    end, lists:zip(ToolCalls, ToolResults)),
    
    % Create the final response
    Response = #{
        message => InitialContent,
        tool_calls => ToolCalls,
        tool_results => ToolResults
    },
    
    StreamPid ! {stream_complete, Response},
    {ok, Response, State}.

%% Execute tool calls in parallel
execute_tool_calls_parallel(ToolCalls) ->
    Parent = self(),
    
    % Spawn workers for each tool call
    Workers = lists:map(fun(ToolCall) ->
        spawn_link(fun() ->
            Result = execute_single_tool_call(ToolCall),
            Parent ! {tool_result, self(), Result}
        end)
    end, ToolCalls),
    
    % Collect results in order
    collect_results(Workers, []).

%% Execute a single tool call
execute_single_tool_call(ToolCall) ->
    try
        FuncInfo = maps:get(<<"function">>, ToolCall, #{}),
        ToolName = maps:get(<<"name">>, FuncInfo, <<"">>),
        ArgsJson = maps:get(<<"arguments">>, FuncInfo, <<"{}">>),
        
        ?LOG_DEBUG("[STREAM_FUNC] Executing tool: ~s with args: ~s", [ToolName, ArgsJson]),
        
        % Decode arguments
        Args = try
            jsx:decode(ArgsJson, [return_maps])
        catch
            _:_ -> #{}
        end,
        
        % Convert tool name to atom
        ToolNameAtom = try
            binary_to_existing_atom(ToolName, utf8)
        catch
            _:_ -> binary_to_atom(ToolName, utf8)
        end,
        
        % Execute the tool
        agent_tools:execute_tool(ToolNameAtom, Args)
    catch
        Type:Error ->
            ?LOG_ERROR("[STREAM_FUNC] Tool execution failed: ~p:~p", [Type, Error]),
            {error, Error}
    end.

%% Collect results from parallel workers
collect_results([], Results) ->
    lists:reverse(Results);
collect_results([Worker | Rest], Results) ->
    receive
        {tool_result, Worker, Result} ->
            collect_results(Rest, [Result | Results])
    after 30000 ->
        collect_results(Rest, [{error, timeout} | Results])
    end.

%% High-level handler for Responses API streaming
handle_responses_api_stream(StreamPid, Input, State) ->
    ?LOG_INFO("[STREAM_HANDLER] Starting Responses API stream for PID: ~p", [StreamPid]),
    Accumulator = init_accumulator(),
    handle_responses_stream_loop(StreamPid, Input, State, Accumulator).

handle_responses_stream_loop(StreamPid, Input, State, Accumulator) ->
    receive
        {stream_event, Event} ->
            ?LOG_INFO("[STREAM_HANDLER] Received stream_event: ~p", [Event]),
            case process_stream_event(Event, Accumulator, StreamPid) of
                {continue, NewAcc} ->
                    handle_responses_stream_loop(StreamPid, Input, State, NewAcc);
                {done, FinalAcc} ->
                    finalize_stream(FinalAcc, StreamPid, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {stream_error, Reason} ->
            ?LOG_ERROR("[STREAM_HANDLER] Received stream_error: ~p", [Reason]),
            StreamPid ! {stream_error, Reason},
            {error, Reason};
        stream_complete ->
            ?LOG_INFO("[STREAM_HANDLER] Received stream_complete"),
            finalize_stream(Accumulator, StreamPid, State);
        Other ->
            ?LOG_WARNING("[STREAM_HANDLER] Unexpected message: ~p", [Other]),
            handle_responses_stream_loop(StreamPid, Input, State, Accumulator)
    after 60000 ->
        ?LOG_ERROR("[STREAM_HANDLER] Timeout waiting for stream events"),
        StreamPid ! {stream_error, timeout},
        {error, timeout}
    end.

%% High-level handler for Chat API streaming
handle_chat_api_stream(StreamPid, Messages, State) ->
    Accumulator = init_accumulator(),
    handle_chat_stream_loop(StreamPid, Messages, State, Accumulator).

handle_chat_stream_loop(StreamPid, Messages, State, Accumulator) ->
    receive
        {stream_chunk, Event} ->
            case process_stream_event(Event, Accumulator, StreamPid) of
                {continue, NewAcc} ->
                    handle_chat_stream_loop(StreamPid, Messages, State, NewAcc);
                {done, FinalAcc} ->
                    finalize_stream(FinalAcc, StreamPid, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        {stream_error, Reason} ->
            StreamPid ! {stream_error, Reason},
            {error, Reason};
        stream_complete ->
            finalize_stream(Accumulator, StreamPid, State)
    after 30000 ->
        StreamPid ! {stream_error, timeout},
        {error, timeout}
    end.

%% Safe logging function with fallback
log_safe(Level, Msg) ->
    try
        case Level of
            info -> colored_logger:data(processed, Msg);
            error -> colored_logger:fire(inferno, Msg);
            warning -> colored_logger:alarm(medium, Msg);
            debug -> colored_logger:system(network, Msg);
            _ -> colored_logger:info(general, Msg)
        end
    catch
        _:_ ->
            % Fallback to standard logging if colored_logger not available
            Prefix = case Level of
                error -> "[ERROR] ";
                warning -> "[WARNING] ";
                debug -> "[DEBUG] ";
                _ -> "[INFO] "
            end,
            % Safe formatting to handle Unicode and special characters
            try
                SafeMsg = case Msg of
                    M when is_list(M) -> 
                        % Convert to binary first to handle Unicode properly
                        case catch unicode:characters_to_binary(M) of
                            B when is_binary(B) -> binary_to_list(B);
                            _ -> lists:flatten(io_lib:format("~w", [M]))
                        end;
                    M when is_binary(M) -> binary_to_list(M);
                    M -> lists:flatten(io_lib:format("~w", [M]))
                end,
                io:format("~s~s~n", [Prefix, SafeMsg])
            catch
                _:_ -> 
                    io:format("~s[LOG_ERROR]~n", [Prefix])
            end
    end.

%% Process tokens for proper display - CRITICAL: Stream TOKENS not BINARY BYTES!
process_token_for_display(Token) when is_binary(Token) ->
    % IMPORTANT: Ensure we're streaming readable text, not binary byte sequences
    % Convert \n sequences to actual newlines for proper display
    ProcessedToken = case binary:replace(Token, <<"\\n">>, <<"\n">>, [global]) of
        Token -> Token; % No changes needed
        Changed -> Changed
    end,
    
    % Validate it's displayable UTF-8 text, not raw bytes
    case unicode:characters_to_binary(ProcessedToken, utf8) of
        ProcessedToken -> ProcessedToken; % Valid UTF-8
        {error, _, _} -> 
            % Invalid UTF-8, convert carefully to avoid showing byte sequences
            iolist_to_binary(io_lib:format("~ts", [ProcessedToken]));
        {incomplete, _, _} ->
            % Incomplete UTF-8, handle gracefully
            ProcessedToken
    end;
process_token_for_display(Token) ->
    % Convert non-binary tokens to displayable text (NOT byte sequences!)
    case is_list(Token) of
        true -> 
            % Check if it's a valid UTF-8 string first
            case unicode:characters_to_binary(Token) of
                {error, _, _} ->
                    % Not a valid UTF-8 string, check if it's ASCII byte list
                    case lists:all(fun(X) -> is_integer(X) andalso X >= 32 andalso X =< 126 end, Token) of
                        true ->
                            % It's printable ASCII, convert to readable text
                            list_to_binary(Token);
                        false ->
                            % Contains non-printable or non-ASCII bytes
                            % Convert each byte to its character if printable, otherwise show as character
                            ReadableChars = lists:map(fun(Byte) ->
                                if 
                                    Byte >= 32 andalso Byte =< 126 -> Byte;  % Printable ASCII
                                    Byte >= 160 andalso Byte =< 255 -> Byte; % Extended ASCII
                                    true -> $?  % Replace non-printable with ?
                                end
                            end, Token),
                            list_to_binary(ReadableChars)
                    end;
                {incomplete, _, _} ->
                    % Incomplete UTF-8, try safe conversion
                    try
                        list_to_binary(Token)
                    catch
                        _:_ -> <<"[invalid_token]">>
                    end;
                BinaryToken when is_binary(BinaryToken) ->
                    % Valid UTF-8 string, process it
                    process_token_for_display(BinaryToken)
            end;
        false ->
            % For non-list, non-binary types, convert to readable text
            case Token of
                T when is_atom(T) ->
                    atom_to_binary(T, utf8);
                T when is_integer(T) ->
                    integer_to_binary(T);
                T when is_float(T) ->
                    float_to_binary(T);
                _ ->
                    % For other types, create readable representation (no raw bytes!)
                    try
                        iolist_to_binary(io_lib:format("~ts", [Token]))
                    catch
                        _:_ -> <<"[complex_token]">>
                    end
            end
    end.

%% Serialize streaming token using jiffy for consistent JSON output
serialize_streaming_token(Token) ->
    ProcessedToken = process_token_for_display(Token),
    StreamingData = #{
        <<"type">> => <<"token">>,
        <<"content">> => ProcessedToken,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    try
        jiffy:encode(StreamingData, [
            {escape_forward_slashes, false},
            {use_nil, false},
            force_utf8
        ])
    catch
        error:Reason ->
            ?LOG_ERROR("[STREAM_FUNC] Failed to serialize token: ~p", [Reason]),
            % Fallback to basic JSON structure
            jiffy:encode(#{
                <<"type">> => <<"token">>,
                <<"content">> => <<"[serialization_error]">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            })
    end.

%% Serialize streaming response using jiffy
serialize_streaming_response(Response) ->
    StreamingData = case Response of
        #{} -> 
            Response#{<<"timestamp">> => erlang:system_time(millisecond)};
        {Type, Content} ->
            #{
                <<"type">> => erlang:atom_to_binary(Type, utf8),
                <<"content">> => Content,
                <<"timestamp">> => erlang:system_time(millisecond)
            };
        Content when is_binary(Content); is_list(Content) ->
            #{
                <<"type">> => <<"content">>,
                <<"content">> => process_token_for_display(Content),
                <<"timestamp">> => erlang:system_time(millisecond)
            };
        Other ->
            #{
                <<"type">> => <<"data">>,
                <<"content">> => iolist_to_binary(io_lib:format("~p", [Other])),
                <<"timestamp">> => erlang:system_time(millisecond)
            }
    end,
    try
        jiffy:encode(StreamingData, [
            {escape_forward_slashes, false},
            {use_nil, false},
            force_utf8
        ])
    catch
        error:Reason ->
            ?LOG_ERROR("[STREAM_FUNC] Failed to serialize response: ~p", [Reason]),
            % Fallback response
            jiffy:encode(#{
                <<"type">> => <<"error">>,
                <<"content">> => <<"Serialization failed">>,
                <<"timestamp">> => erlang:system_time(millisecond),
                <<"error">> => iolist_to_binary(io_lib:format("~p", [Reason]))
            })
    end.

%% Helper function to convert atom to binary
atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
atom_to_binary(Binary) when is_binary(Binary) ->
    Binary;
atom_to_binary(Other) ->
    iolist_to_binary(io_lib:format("~w", [Other])).