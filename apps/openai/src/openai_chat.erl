%% openai_chat.erl
%% OpenAI Chat Completions API client
-module(openai_chat).
-behaviour(gen_server).

%% API exports
-export([
    start_link/1,
    create_chat_completion/3,
    create_streaming_completion/3
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
-define(DEFAULT_TIMEOUT, 60000).

-record(state, {
    api_key = undefined :: binary() | undefined,
    base_url = <<"https://api.openai.com/v1">> :: binary(),
    rate_limiter = undefined :: pid() | undefined,
    options = #{} :: map()
}).

%% Public API

%% Start the chat client
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Create a chat completion
create_chat_completion(Model, Messages, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_chat_completion, Model, Messages, Options}, Timeout).

%% Create a streaming chat completion
create_streaming_completion(Model, Messages, Options) ->
    Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
    gen_server:call(?SERVER, {create_streaming_completion, Model, Messages, Options}, Timeout).

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

handle_call({create_chat_completion, Model, Messages, Options}, _From, State) ->
    case do_create_chat_completion(Model, Messages, Options, State) of
        {ok, Response} ->
            {reply, {ok, Response}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({create_streaming_completion, Model, Messages, Options}, {From, _}, State) ->
    spawn(fun() -> 
        do_create_streaming_completion(Model, Messages, Options#{stream => true}, From, State) 
    end),
    {reply, ok, State};

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

do_create_chat_completion(Model, Messages, Options, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/chat/completions">>,
    
    % Build request body
    Body = #{
        <<"model">> => Model,
        <<"messages">> => Messages
    },
    
    % Add optional parameters
    Body1 = case maps:get(temperature, Options, undefined) of
        undefined -> Body;
        Temp -> Body#{<<"temperature">> => Temp}
    end,
    Body2 = case maps:get(max_tokens, Options, undefined) of
        undefined -> Body1;
        MaxTokens -> Body1#{<<"max_tokens">> => MaxTokens}
    end,
    Body3 = case maps:get(stream, Options, undefined) of
        undefined -> Body2;
        Stream -> Body2#{<<"stream">> => Stream}
    end,
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body3),
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody}, 
                      [{timeout, 60000}, {connect_timeout, 5000}], [{body_format, binary}]) of
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
do_create_streaming_completion(Model, Messages, Options, CallerPid, #state{api_key = ApiKey, base_url = BaseUrl}) ->
    Url = <<BaseUrl/binary, "/chat/completions">>,
    
    % Build request body with stream enabled
    Body = #{
        <<"model">> => Model,
        <<"messages">> => Messages,
        <<"stream">> => true
    },
    
    % Add optional parameters
    Body1 = case maps:get(temperature, Options, undefined) of
        undefined -> Body;
        Temp -> Body#{<<"temperature">> => Temp}
    end,
    Body2 = case maps:get(max_tokens, Options, undefined) of
        undefined -> Body1;
        MaxTokens -> Body1#{<<"max_tokens">> => MaxTokens}
    end,
    
    % Encode body to JSON
    JsonBody = jsx:encode(Body2),
    
    % Build headers
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
        {"Content-Type", "application/json"}
    ],
    
    % Make HTTP request with streaming
    case httpc:request(post, {binary_to_list(Url), Headers, "application/json", JsonBody},
                      [{timeout, 60000}, {connect_timeout, 5000}], 
                      [{sync, false}, {stream, self}, {body_format, binary}]) of
        {ok, RequestId} ->
            handle_stream_response(RequestId, CallerPid, <<>>);
        {error, Reason} ->
            gen_server:reply(CallerPid, {error, {request_failed, Reason}})
    end.

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
                CallerPid ! {stream_chunk, Event}
            end, Events),
            
            % Continue streaming
            handle_stream_response(RequestId, CallerPid, ProcessedBuffer);
            
        {http, {RequestId, stream_end, _Headers}} ->
            % Process any remaining data in buffer
            {_, Events} = process_sse_events(<<Buffer/binary, "\n\n">>),
            lists:foreach(fun(Event) ->
                CallerPid ! {stream_chunk, Event}
            end, Events),
            CallerPid ! stream_complete;
            
        {http, {RequestId, {error, Reason}}} ->
            CallerPid ! {stream_error, Reason}
            
    after 30000 ->
        CallerPid ! {stream_error, timeout}
    end.

%% Process Server-Sent Events (SSE) format
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
                    process_sse_lines(Rest, [], Events);
                _ ->
                    % Try to parse JSON
                    case catch jsx:decode(Data, [return_maps]) of
                        {'EXIT', _} ->
                            % Incomplete JSON, keep accumulating
                            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events);
                        Decoded ->
                            % Extract content from delta
                            Event = extract_stream_content(Decoded),
                            process_sse_lines(Rest, [], [Event | Events])
                    end
            end;
        <<>> ->
            % Empty line, potential event boundary
            process_sse_lines(Rest, Acc, Events);
        _ ->
            % Other lines, keep accumulating
            process_sse_lines(Rest, [Line, <<"\n">> | Acc], Events)
    end.

extract_stream_content(Data) ->
    case maps:get(<<"choices">>, Data, []) of
        [] -> <<>>;
        [Choice | _] ->
            case maps:get(<<"delta">>, Choice, #{}) of
                #{<<"content">> := Content} when Content =/= null -> Content;
                _ -> <<>>
            end
    end.