%% openai_streaming.erl
%% Streaming utilities for OpenAI API calls with semantic events support
-module(openai_streaming).

-export([
    enable_chat_streaming/2,
    enable_responses_streaming/2,
    create_streaming_chat/3,
    create_streaming_response/3,
    handle_stream_events/2,
    format_stream_event/1
]).

%% Logging macros with fallback
-define(LOG_INFO(Msg), log_safe(info, Msg)).
-define(LOG_INFO(Msg, Args), log_safe(info, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), log_safe(error, Msg)).
-define(LOG_ERROR(Msg, Args), log_safe(error, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), log_safe(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), log_safe(success, io_lib:format(Msg, Args))).

%% Enable streaming for Chat Completions API
enable_chat_streaming(Options, EventHandler) ->
    StreamOptions = Options#{stream => true},
    ?LOG_INFO("[STREAMING] 🌊 Enabled streaming for Chat Completions API"),
    {StreamOptions, EventHandler}.

%% Enable streaming for Responses API
enable_responses_streaming(Options, EventHandler) ->
    StreamOptions = Options#{stream => true},
    ?LOG_INFO("[STREAMING] 🌊 Enabled streaming for Responses API"),
    {StreamOptions, EventHandler}.

%% Create streaming chat completion with event handling
create_streaming_chat(Model, Messages, Options) ->
    ?LOG_INFO("[STREAMING] 🚀 Starting streaming chat completion"),
    ?LOG_INFO("[STREAMING] Model: ~p, Messages: ~p", [Model, length(Messages)]),
    
    % Enable streaming
    StreamOptions = Options#{stream => true},
    
    % Start streaming process
    CallerPid = self(),
    StreamPid = spawn(fun() ->
        case openai_chat:create_streaming_completion(Model, Messages, StreamOptions) of
            ok ->
                ?LOG_SUCCESS("[STREAMING] ✅ Streaming chat started successfully"),
                stream_chat_loop(CallerPid);
            {error, Reason} ->
                ?LOG_ERROR("[STREAMING] ❌ Failed to start streaming: ~p", [Reason]),
                CallerPid ! {stream_error, Reason}
        end
    end),
    
    {ok, StreamPid}.

%% Create streaming response with event handling
create_streaming_response(Input, Model, Options) ->
    ?LOG_INFO("[STREAMING] 🚀 Starting streaming response"),
    ?LOG_INFO("[STREAMING] Model: ~p, Input items: ~p", [Model, length(Input)]),
    
    % Enable streaming
    StreamOptions = Options#{stream => true},
    
    % Start streaming process
    CallerPid = self(),
    StreamPid = spawn(fun() ->
        case openai_responses:create_streaming_response(Input, Model, StreamOptions) of
            ok ->
                ?LOG_SUCCESS("[STREAMING] ✅ Streaming response started successfully"),
                stream_responses_loop(CallerPid);
            {error, Reason} ->
                ?LOG_ERROR("[STREAMING] ❌ Failed to start streaming: ~p", [Reason]),
                CallerPid ! {stream_error, Reason}
        end
    end),
    
    {ok, StreamPid}.

%% Handle stream events with callback processing
handle_stream_events(EventHandler, Timeout) ->
    handle_stream_events(EventHandler, Timeout, []).

handle_stream_events(EventHandler, Timeout, AccumulatedEvents) ->
    receive
        {stream_chunk, Event} ->
            ?LOG_INFO("[STREAMING] 📥 Received stream chunk: ~p", [maps:get(type, Event, unknown)]),
            
            % Process event with handler
            case EventHandler(Event) of
                continue ->
                    handle_stream_events(EventHandler, Timeout, [Event | AccumulatedEvents]);
                {continue, NewTimeout} ->
                    handle_stream_events(EventHandler, NewTimeout, [Event | AccumulatedEvents]);
                stop ->
                    ?LOG_SUCCESS("[STREAMING] 🛑 Stream processing stopped by handler"),
                    {ok, lists:reverse([Event | AccumulatedEvents])};
                {error, Reason} ->
                    ?LOG_ERROR("[STREAMING] ❌ Event handler error: ~p", [Reason]),
                    {error, Reason}
            end;
            
        {stream_event, Event} ->
            ?LOG_INFO("[STREAMING] 📥 Received semantic event: ~s", [maps:get(<<"type">>, Event, <<"unknown">>)]),
            
            % Process semantic event
            case EventHandler(Event) of
                continue ->
                    handle_stream_events(EventHandler, Timeout, [Event | AccumulatedEvents]);
                {continue, NewTimeout} ->
                    handle_stream_events(EventHandler, NewTimeout, [Event | AccumulatedEvents]);
                stop ->
                    ?LOG_SUCCESS("[STREAMING] 🛑 Stream processing stopped by handler"),
                    {ok, lists:reverse([Event | AccumulatedEvents])};
                {error, Reason} ->
                    ?LOG_ERROR("[STREAMING] ❌ Event handler error: ~p", [Reason]),
                    {error, Reason}
            end;
            
        stream_complete ->
            ?LOG_SUCCESS("[STREAMING] ✅ Stream completed successfully"),
            {ok, lists:reverse(AccumulatedEvents)};
            
        {stream_error, Error} ->
            ?LOG_ERROR("[STREAMING] ❌ Stream error: ~p", [Error]),
            {error, Error}
            
    after Timeout ->
        ?LOG_ERROR("[STREAMING] ⏰ Stream timeout after ~pms", [Timeout]),
        {error, timeout}
    end.

%% Format stream event for display or processing
format_stream_event(#{type := content, data := Data}) ->
    #{type => <<"text_delta">>, content => Data, formatted => true};
format_stream_event(#{type := text_delta, data := Data}) ->
    #{type => <<"text_delta">>, content => Data, formatted => true};
format_stream_event(#{type := tool_calls, data := ToolCalls, index := Index}) ->
    #{type => <<"tool_calls">>, tool_calls => ToolCalls, index => Index, formatted => true};
format_stream_event(#{type := function_call, data := FuncCall, index := Index}) ->
    #{type => <<"function_call">>, function_call => FuncCall, index => Index, formatted => true};
format_stream_event(#{type := role, data := Role}) ->
    #{type => <<"role_assignment">>, role => Role, formatted => true};
format_stream_event(#{type := finish, reason := Reason}) ->
    #{type => <<"completion_finished">>, finish_reason => Reason, formatted => true};
format_stream_event(#{type := response_created} = Event) ->
    Event#{formatted => true};
format_stream_event(#{type := response_completed} = Event) ->
    Event#{formatted => true};
format_stream_event(#{type := output_item_added} = Event) ->
    Event#{formatted => true};
format_stream_event(#{type := content_part_added} = Event) ->
    Event#{formatted => true};
format_stream_event(#{<<"type">> := Type} = Event) ->
    % Semantic event from Responses API
    Event#{formatted => true, semantic_type => Type};
format_stream_event(Event) ->
    % Unknown event type
    Event#{type => <<"unknown">>, formatted => true}.

%% Safe logging function with fallback
log_safe(Level, Msg) ->
    try
        case Level of
            info -> colored_logger:data(processed, Msg);
            error -> colored_logger:fire(inferno, Msg);
            success -> colored_logger:complete(success, Msg);
            _ -> colored_logger:info(general, Msg)
        end
    catch
        _:_ ->
            % Fallback to standard logging if colored_logger not available
            Prefix = case Level of
                error -> "[ERROR] ";
                success -> "[SUCCESS] ";
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

%% Internal functions

stream_chat_loop(CallerPid) ->
    receive
        {stream_chunk, Event} ->
            CallerPid ! {stream_chunk, Event},
            stream_chat_loop(CallerPid);
        stream_complete ->
            CallerPid ! stream_complete;
        {stream_error, Error} ->
            CallerPid ! {stream_error, Error}
    after 30000 ->
        CallerPid ! {stream_error, timeout}
    end.

stream_responses_loop(CallerPid) ->
    receive
        {stream_event, Event} ->
            CallerPid ! {stream_event, Event},
            stream_responses_loop(CallerPid);
        stream_complete ->
            CallerPid ! stream_complete;
        {stream_error, Error} ->
            CallerPid ! {stream_error, Error}
    after 60000 ->
        CallerPid ! {stream_error, timeout}
    end.