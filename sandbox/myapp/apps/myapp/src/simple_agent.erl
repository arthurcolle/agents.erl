%% simple_agent.erl
%% A simplified interface for the Erlang agent framework
-module(simple_agent).

%% API exports
-export([
    start/0,
    stop/0,
    chat/1,
    chat/2,
    chat_stream/1,
    chat_stream/2
]).

%% @doc Start the application
-spec start() -> ok | {error, term()}.
start() ->
    % Start the OpenAI application
    case application:ensure_all_started(openai) of
        {ok, _} -> ok;
        Error -> Error
    end.

%% @doc Stop the application
-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(agent).

%% @doc Send a chat message with default settings
-spec chat(binary()) -> binary() | {error, term()}.
chat(Message) ->
    chat(Message, #{}).

%% @doc Send a chat message with optional settings
-spec chat(binary(), map()) -> binary() | {error, term()}.
chat(Message, Options) ->
    % Get model and system message from options
    Model = maps:get(model, Options, <<"gpt-4o-mini">>),
    SystemMsg = maps:get(system_message, Options, 
        <<"You are a helpful assistant.">>),
    
    % Build messages array for OpenAI API
    Messages = [
        #{<<"role">> => <<"system">>, <<"content">> => SystemMsg},
        #{<<"role">> => <<"user">>, <<"content">> => Message}
    ],
    
    % Call OpenAI API
    case openai_chat:create_chat_completion(Model, Messages, Options) of
        {ok, Response} ->
            extract_message_content(Response);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Send a streaming chat message with default settings
-spec chat_stream(binary()) -> ok.
chat_stream(Message) ->
    chat_stream(Message, #{}).

%% @doc Send a streaming chat message with optional settings
-spec chat_stream(binary(), map()) -> ok.
chat_stream(Message, Options) ->
    % Get model and system message from options
    Model = maps:get(model, Options, <<"gpt-4o-mini">>),
    SystemMsg = maps:get(system_message, Options, 
        <<"You are a helpful assistant.">>),
    
    % Build messages array for OpenAI API
    Messages = [
        #{<<"role">> => <<"system">>, <<"content">> => SystemMsg},
        #{<<"role">> => <<"user">>, <<"content">> => Message}
    ],
    
    % Call OpenAI streaming API
    ok = openai_chat:create_streaming_completion(Model, Messages, Options),
    
    % Handle streaming response
    handle_stream_response().

%% Helper function to handle streaming response
handle_stream_response() ->
    receive
        {stream_chunk, Content} ->
            io:format("~s", [Content]),
            handle_stream_response();
            
        stream_complete ->
            io:format("~n"),
            ok;
            
        {stream_error, Reason} ->
            io:format("~nStreaming error: ~p~n", [Reason]),
            {error, Reason}
            
    after 30000 ->
        io:format("~nStreaming timeout~n"),
        {error, timeout}
    end.

%% Helper function to extract content from response
extract_message_content(Response) ->
    try
        case maps:get(<<"choices">>, Response, []) of
            [] -> <<"No response generated">>;
            Choices ->
                FirstChoice = hd(Choices),
                case maps:get(<<"message">>, FirstChoice, #{}) of
                    #{<<"content">> := Content} when Content =/= null -> Content;
                    _ -> <<"No content in response">>
                end
        end
    catch
        _:_ -> <<"Error extracting response content">>
    end.