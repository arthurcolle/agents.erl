%% openai_responses.erl
%% OpenAI Responses API client - Advanced interface for model responses
-module(openai_responses).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_response/2,
    create_response/3,
    create_streaming_response/2,
    create_streaming_response/3,
    get_response/1,
    get_response/2,
    delete_response/1,
    cancel_response/1,
    list_input_items/1,
    list_input_items/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 120000).

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_DEBUG(Msg), colored_logger:system(network, Msg)).
-define(LOG_DEBUG(Msg, Args), colored_logger:system(network, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).

-record(state, {
    api_key = undefined :: binary() | undefined,
    base_url = <<"https://api.openai.com/v1">> :: binary(),
    rate_limiter = undefined :: pid() | undefined,
    options = #{} :: map()
}).

%% Public API

%% Start the responses client
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Create a response
create_response(Input, Model) ->
    create_response(Input, Model, #{}).

create_response(Input, Model, Options) ->
    ?LOG_INFO("[RESPONSES_API] 📨 Create response request - Model: ~p", [Model]),
    ?LOG_DEBUG("[RESPONSES_API] Input items: ~p", [length(Input)]),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_response, Input, Model, Options}, Timeout).

%% Create a streaming response
create_streaming_response(Input, Model) ->
    create_streaming_response(Input, Model, #{}).

create_streaming_response(Input, Model, Options) ->
    ?LOG_INFO("[RESPONSES_API] 🌊 Create streaming response - Model: ~p", [Model]),
    ?LOG_DEBUG("[RESPONSES_API] Input items: ~p", [length(Input)]),
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_streaming_response, Input, Model, Options}, Timeout).

%% Get a response by ID
get_response(ResponseId) ->
    get_response(ResponseId, #{}).

get_response(ResponseId, Options) ->
    gen_server:call(?SERVER, {get_response, ResponseId, Options}).

%% Delete a response
delete_response(ResponseId) ->
    gen_server:call(?SERVER, {delete_response, ResponseId}).

%% Cancel a response
cancel_response(ResponseId) ->
    gen_server:call(?SERVER, {cancel_response, ResponseId}).

%% List input items for a response
list_input_items(ResponseId) ->
    list_input_items(ResponseId, #{}).

list_input_items(ResponseId, Options) ->
    gen_server:call(?SERVER, {list_input_items, ResponseId, Options}).

%% gen_server callbacks

init(Options) ->
    % Ensure inets application is started for httpc
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, _} = Error -> error(Error)
    end,
    
    % Get API key from environment
    ApiKey = case os:getenv("OPENAI_API_KEY") of
        false -> 
            error({missing_api_key, "OPENAI_API_KEY environment variable not set"});
        Key -> 
            list_to_binary(Key)
    end,
    
    % Set up rate limiter
    RateLimiter = case whereis(openai_rate_limiter) of
        undefined -> undefined;
        Pid -> Pid
    end,
    
    {ok, #state{
        api_key = ApiKey,
        rate_limiter = RateLimiter,
        options = Options
    }}.

handle_call({create_response, Input, Model, Options}, From, State) ->
    ?LOG_INFO("[RESPONSES_API] 📥 Processing create_response from ~p", [From]),
    StartTime = erlang:monotonic_time(millisecond),
    
    case do_create_response(Input, Model, Options, State) of
        {ok, Response} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ResponseId = maps:get(<<"id">>, Response, <<"unknown">>),
            ?LOG_SUCCESS("[RESPONSES_API] ✅ Response ~s created in ~pms", [ResponseId, Duration]),
            {reply, {ok, Response}, State};
        {error, Reason} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            ?LOG_ERROR("[RESPONSES_API] ❌ Create response failed after ~pms: ~p", [Duration, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({create_streaming_response, Input, Model, Options}, {From, _}, State) ->
    ?LOG_INFO("[RESPONSES_API] 🌊 Processing streaming response from ~p", [From]),
    WorkerPid = spawn(fun() -> 
        ?LOG_INFO("[RESPONSES_API] 👷 Streaming worker started for ~p", [From]),
        do_create_streaming_response(Input, Model, Options#{stream => true}, From, State) 
    end),
    ?LOG_INFO("[RESPONSES_API] Spawned streaming worker: ~p", [WorkerPid]),
    {reply, ok, State};

handle_call({get_response, ResponseId, Options}, _From, State) ->
    case do_get_response(ResponseId, Options, State) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete_response, ResponseId}, _From, State) ->
    case do_delete_response(ResponseId, State) of
        {ok, Result} ->
            {reply, {ok, Result}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_response, ResponseId}, _From, State) ->
    case do_cancel_response(ResponseId, State) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({list_input_items, ResponseId, Options}, _From, State) ->
    case do_list_input_items(ResponseId, Options, State) of
        {ok, Items} ->
            {reply, {ok, Items}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

do_create_response(Input, Model, Options, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    ?LOG_DEBUG("[RESPONSES_API] Building request for model: ~p", [Model]),
    Url = <<BaseUrl/binary, "/responses">>,
    
    NormalizedInput = normalize_input(Input),
    InputCount = length(NormalizedInput),
    ?LOG_DEBUG("[RESPONSES_API] Normalized input: ~p items", [InputCount]),
    
    % Build request body
    Body = #{
        <<"input">> => NormalizedInput,
        <<"model">> => Model
    },
    
    % Add optional parameters
    Body1 = add_optional_params(Body, Options),
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body1),
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody}, 
                      [{timeout, 120000}, {connect_timeout, 10000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            % Try to parse error response
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Streaming implementation
do_create_streaming_response(Input, Model, Options, CallerPid, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/responses">>,
    
    % Build request body with stream enabled
    Body = #{
        <<"input">> => normalize_input(Input),
        <<"model">> => Model,
        <<"stream">> => true
    },
    
    % Add optional parameters
    Body1 = add_optional_params(Body, Options),
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body1),
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request with streaming
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody},
                      [{timeout, 120000}, {connect_timeout, 10000}], 
                      [{sync, false}, {stream, self}, {body_format, binary}]) of
        {ok, RequestId} ->
            handle_stream_response(RequestId, CallerPid, <<>>);
        {error, Reason} ->
            gen_server:reply(CallerPid, {error, {request_failed, Reason}})
    end.

do_get_response(ResponseId, Options, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/responses/", ResponseId/binary>>,
    
    % Build query parameters
    QueryParams = build_query_params(Options),
    FullUrl = case QueryParams of
        <<>> -> Url;
        _ -> <<Url/binary, "?", QueryParams/binary>>
    end,
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(get, {binary_to_list(FullUrl), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

do_delete_response(ResponseId, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/responses/", ResponseId/binary>>,
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(delete, {binary_to_list(Url), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

do_cancel_response(ResponseId, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/responses/", ResponseId/binary, "/cancel">>,
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", "{}"}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

do_list_input_items(ResponseId, Options, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/responses/", ResponseId/binary, "/input_items">>,
    
    % Build query parameters
    QueryParams = build_query_params(Options),
    FullUrl = case QueryParams of
        <<>> -> Url;
        _ -> <<Url/binary, "?", QueryParams/binary>>
    end,
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(get, {binary_to_list(FullUrl), Headers}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                Response when is_map(Response) ->
                    {ok, Response};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            case catch jsx:decode(ResponseBody, [return_maps]) of
                #{<<"error">> := Error} ->
                    {error, {api_error, StatusCode, Error}};
                _ ->
                    {error, {http_error, StatusCode, ResponseBody}}
            end;
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%% Helper functions

normalize_input(Input) when is_binary(Input) ->
    Input;
normalize_input(Input) when is_list(Input) ->
    Input;
normalize_input(Input) when is_map(Input) ->
    Input;
normalize_input(Input) ->
    % Convert to string if not already
    iolist_to_binary(io_lib:format("~p", [Input])).

%% Enhanced input normalization with file support
normalize_input_with_files(Input) when is_list(Input) ->
    lists:map(fun normalize_input_item/1, Input);
normalize_input_with_files(Input) ->
    normalize_input_item(Input).

normalize_input_item(#{<<"type">> := <<"image_file">>, <<"image_file">> := #{<<"file_id">> := FileId}} = Item) ->
    % Image file input
    ?LOG_INFO("[FILE_INPUT] 🖼️  Adding image file: ~s", [FileId]),
    Item;
normalize_input_item(#{<<"type">> := <<"text">>, <<"text">> := Text} = Item) when is_binary(Text) ->
    % Text input
    Item;
normalize_input_item(#{<<"role">> := Role, <<"content">> := Content} = Item) ->
    % Chat-style message input
    NormalizedContent = normalize_content_with_files(Content),
    Item#{<<"content">> => NormalizedContent};
normalize_input_item(Input) when is_binary(Input) ->
    % Simple text input
    #{<<"type">> => <<"text">>, <<"text">> => Input};
normalize_input_item(Input) when is_map(Input) ->
    % Pass through other map inputs
    Input;
normalize_input_item(Input) ->
    % Convert other types to text
    Text = iolist_to_binary(io_lib:format("~p", [Input])),
    #{<<"type">> => <<"text">>, <<"text">> => Text}.

normalize_content_with_files(Content) when is_list(Content) ->
    lists:map(fun normalize_content_part/1, Content);
normalize_content_with_files(Content) when is_binary(Content) ->
    Content;
normalize_content_with_files(Content) ->
    iolist_to_binary(io_lib:format("~p", [Content])).

normalize_content_part(#{<<"type">> := <<"image_file">>, <<"image_file">> := #{<<"file_id">> := FileId}} = Part) ->
    ?LOG_INFO("[FILE_INPUT] 🖼️  Adding image file in content: ~s", [FileId]),
    Part;
normalize_content_part(#{<<"type">> := <<"text">>, <<"text">> := Text} = Part) ->
    Part;
normalize_content_part(Part) when is_map(Part) ->
    Part;
normalize_content_part(Text) when is_binary(Text) ->
    #{<<"type">> => <<"text">>, <<"text">> => Text};
normalize_content_part(Other) ->
    Text = iolist_to_binary(io_lib:format("~p", [Other])),
    #{<<"type">> => <<"text">>, <<"text">> => Text}.

add_optional_params(Body, Options) ->
    lists:foldl(fun({Key, DefaultValue}, Acc) ->
        case maps:get(Key, Options, DefaultValue) of
            DefaultValue -> Acc;
            Value when Key =:= input -> 
                % Handle file inputs specially
                NormalizedValue = normalize_input_with_files(Value),
                Acc#{atom_to_binary(Key) => NormalizedValue};
            Value -> Acc#{atom_to_binary(Key) => Value}
        end
    end, Body, [
        {background, undefined},
        {include, undefined},
        {instructions, undefined},
        {max_output_tokens, undefined},
        {metadata, undefined},
        {parallel_tool_calls, true},
        {previous_response_id, undefined},
        {reasoning, undefined},
        {service_tier, undefined},
        {store, true},
        {stream, undefined},
        {temperature, undefined},
        {text, undefined},
        {tool_choice, undefined},
        {tools, undefined},
        {top_p, undefined},
        {truncation, undefined},
        {user, undefined}
    ]).

build_query_params(Options) ->
    Params = lists:foldl(fun({Key, DefaultValue}, Acc) ->
        case maps:get(Key, Options, DefaultValue) of
            DefaultValue -> Acc;
            Value when Key =:= include, is_list(Value) ->
                % Handle array parameters
                IncludeParams = [io_lib:format("include=~s", [Item]) || Item <- Value],
                [string:join(IncludeParams, "&") | Acc];
            Value ->
                ParamStr = io_lib:format("~s=~s", [Key, format_query_value(Value)]),
                [ParamStr | Acc]
        end
    end, [], [
        {'after', undefined},
        {before, undefined},
        {include, undefined},
        {limit, undefined},
        {order, undefined},
        {starting_after, undefined},
        {stream, undefined}
    ]),
    case Params of
        [] -> <<>>;
        _ -> list_to_binary(string:join(lists:reverse(Params), "&"))
    end.

format_query_value(Value) when is_binary(Value) -> Value;
format_query_value(Value) when is_atom(Value) -> atom_to_list(Value);
format_query_value(Value) when is_integer(Value) -> integer_to_list(Value);
format_query_value(Value) when is_float(Value) -> float_to_list(Value);
format_query_value(Value) when is_boolean(Value) -> 
    case Value of
        true -> "true";
        false -> "false"
    end;
format_query_value(Value) -> io_lib:format("~p", [Value]).

handle_stream_response(RequestId, CallerPid, Buffer) ->
    receive
        {http, {RequestId, stream_start, _Headers}} ->
            handle_stream_response(RequestId, CallerPid, Buffer);
            
        {http, {RequestId, stream, BinBodyPart}} ->
            % Accumulate data and process complete events
            NewBuffer = <<Buffer/binary, BinBodyPart/binary>>,
            {ProcessedBuffer, Events} = process_sse_events(NewBuffer),
            
            % Send events to caller
            lists:foreach(fun(Event) ->
                CallerPid ! {stream_event, Event}
            end, Events),
            
            % Continue streaming
            handle_stream_response(RequestId, CallerPid, ProcessedBuffer);
            
        {http, {RequestId, stream_end, _Headers}} ->
            % Process any remaining data in buffer
            {_, Events} = process_sse_events(<<Buffer/binary, "\n\n">>),
            lists:foreach(fun(Event) ->
                CallerPid ! {stream_event, Event}
            end, Events),
            CallerPid ! stream_complete;
            
        {http, {RequestId, {error, Reason}}} ->
            CallerPid ! {stream_error, Reason}
            
    after 60000 ->
        CallerPid ! {stream_error, timeout}
    end.

%% Process Server-Sent Events (SSE) format for Responses API with semantic events
process_sse_events(Buffer) ->
    Lines = binary:split(Buffer, <<"\n">>, [global]),
    process_sse_lines(Lines, [], []).

process_sse_lines([], Acc, Events) ->
    {iolist_to_binary(lists:reverse(Acc)), lists:reverse(Events)};
process_sse_lines([Line | Rest], Acc, Events) ->
    case Line of
        <<"data: ", Data/binary>> ->
            case Data of
                <<"[DONE]">> ->
                    % Stream is complete
                    DoneEvent = #{type => <<"stream_done">>, timestamp => os:system_time(millisecond)},
                    process_sse_lines(Rest, [], [DoneEvent | Events]);
                _ ->
                    % Try to parse JSON
                    case catch jsx:decode(Data, [return_maps]) of
                        {'EXIT', _} ->
                            % Incomplete JSON, keep accumulating
                            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events);
                        Decoded ->
                            % Process semantic event and add timestamp
                            ProcessedEvent = process_semantic_event(Decoded),
                            process_sse_lines(Rest, [], [ProcessedEvent | Events])
                    end
            end;
        <<>> ->
            % Empty line, potential event boundary
            process_sse_lines(Rest, Acc, Events);
        _ ->
            % Other lines, keep accumulating
            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events)
    end.

%% Process semantic events for the Responses API
process_semantic_event(#{<<"type">> := Type} = Event) ->
    % Add timestamp and normalize event
    Event#{<<"processed_at">> => os:system_time(millisecond)};
process_semantic_event(Event) ->
    % Fallback for non-semantic events
    Event#{<<"type">> => <<"unknown">>, <<"processed_at">> => os:system_time(millisecond)}.