-module(interaction_logger).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([log_button_click/2, log_error/2, log_interaction/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    log_file :: file:io_device() | undefined,
    console_enabled = true :: boolean()
}).

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

log_button_click(Action, Data) ->
    gen_server:cast(?MODULE, {log_button_click, Action, Data}).

log_error(Error, Context) ->
    gen_server:cast(?MODULE, {log_error, Error, Context}).

log_interaction(LogData) ->
    gen_server:cast(?MODULE, {log_interaction, LogData}).

%%% GenServer Callbacks

init([]) ->
    %% Enable colored logging for interactions
    ?LOG_INFO("Starting Interaction Logger"),
    {ok, #state{}}.

handle_cast({log_button_click, Action, Data}, State) ->
    Timestamp = format_timestamp(),
    ButtonText = maps:get(<<"buttonText">>, Data, <<"unknown">>),
    ButtonId = maps:get(<<"buttonId">>, Data, <<"unnamed">>),
    UserAgent = maps:get(<<"userAgent">>, Data, <<"unknown">>),
    Url = maps:get(<<"url">>, Data, <<"unknown">>),
    
    %% Create comprehensive log message with emoji for visibility
    LogMessage = io_lib:format(
        "🖱️  [~s] BUTTON CLICKED: ~s | ID: ~s | Action: ~s | URL: ~s | UserAgent: ~s",
        [Timestamp, ButtonText, ButtonId, Action, Url, UserAgent]
    ),
    
    %% Use colored logger for visibility in console
    colored_logger:user(button_click, lists:flatten(LogMessage)),
    
    %% Also log via standard logger for file output
    ?LOG_INFO("Button Click - Action: ~s, Button: ~s, ID: ~s", [Action, ButtonText, ButtonId]),
    
    {noreply, State};

handle_cast({log_error, ErrorData, Context}, State) ->
    Timestamp = format_timestamp(),
    Message = maps:get(<<"message">>, ErrorData, <<"unknown error">>),
    Severity = maps:get(<<"severity">>, ErrorData, <<"medium">>),
    Url = maps:get(<<"url">>, ErrorData, <<"unknown">>),
    Stack = maps:get(<<"stack">>, ErrorData, undefined),
    
    %% Create error log message with RED color for visibility
    LogMessage = case Stack of
        undefined ->
            io_lib:format(
                "🔴 [~s] ERROR: ~s | Severity: ~s | Context: ~s | URL: ~s",
                [Timestamp, Message, Severity, Context, Url]
            );
        _ ->
            io_lib:format(
                "🔴 [~s] ERROR: ~s | Severity: ~s | Context: ~s | URL: ~s | Stack: ~s",
                [Timestamp, Message, Severity, Context, Url, Stack]
            )
    end,
    
    %% Use colored logger with fire (red) color for errors
    colored_logger:fire(error, lists:flatten(LogMessage)),
    
    %% Also log via standard logger
    ?LOG_ERROR("Frontend Error - Message: ~s, Severity: ~s, Context: ~s", [Message, Severity, Context]),
    
    {noreply, State};

handle_cast({log_interaction, LogData}, State) ->
    Timestamp = format_timestamp(),
    Type = maps:get(<<"type">>, LogData, <<"unknown">>),
    Action = maps:get(<<"action">>, LogData, <<"unknown">>),
    
    case Type of
        <<"button_click">> ->
            handle_cast({log_button_click, Action, LogData}, State);
        <<"error">> ->
            handle_cast({log_error, LogData, maps:get(<<"context">>, LogData, <<"unknown">>)}, State);
        _ ->
            LogMessage = io_lib:format(
                "📊 [~s] INTERACTION: ~s | Action: ~s | Data: ~p",
                [Timestamp, Type, Action, LogData]
            ),
            colored_logger:info(interaction, lists:flatten(LogMessage)),
            ?LOG_INFO("Frontend Interaction - Type: ~s, Action: ~s", [Type, Action]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal Functions

format_timestamp() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
                  [Year, Month, Day, Hour, Minute, Second]).