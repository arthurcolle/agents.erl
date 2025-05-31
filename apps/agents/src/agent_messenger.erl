%% agent_messenger.erl
%% Module for agent-to-agent communication
-module(agent_messenger).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    send_message/3,
    send_broadcast/2,
    register_handler/2,
    unregister_handler/1
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

-record(state, {
    handlers = #{} :: map(),  % Agent ID -> Handler function or PID
    message_history = [] :: list()  % Store recent messages for debugging
}).

%% API Functions

%% Start the messenger service
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% Send a message to a specific agent
-spec send_message(AgentId, Message, Options) -> Result when
    AgentId :: term(),
    Message :: term(),
    Options :: map(),
    Result :: {ok, Reply} | {error, Reason},
    Reply :: term(),
    Reason :: term().
send_message(AgentId, Message, Options) ->
    gen_server:call(?SERVER, {send_message, AgentId, Message, Options}, 30000).

%% Send a broadcast message to all registered agents
-spec send_broadcast(Message, Options) -> Result when
    Message :: term(),
    Options :: map(),
    Result :: {ok, Count} | {error, Reason},
    Count :: non_neg_integer(),
    Reason :: term().
send_broadcast(Message, Options) ->
    gen_server:call(?SERVER, {send_broadcast, Message, Options}, 30000).

%% Register a message handler for an agent
-spec register_handler(AgentId, Handler) -> Result when
    AgentId :: term(),
    Handler :: function() | pid(),
    Result :: ok | {error, Reason},
    Reason :: term().
register_handler(AgentId, Handler) ->
    gen_server:call(?SERVER, {register_handler, AgentId, Handler}).

%% Unregister a message handler
-spec unregister_handler(AgentId) -> Result when
    AgentId :: term(),
    Result :: ok | {error, Reason},
    Reason :: term().
unregister_handler(AgentId) ->
    gen_server:call(?SERVER, {unregister_handler, AgentId}).

%% gen_server callbacks

init(_Options) ->
    % Setup any initialization from options
    % _MaxHistorySize = maps:get(max_history_size, _Options, 100),
    {ok, #state{handlers = #{}, message_history = []}}.

handle_call({register_handler, AgentId, Handler}, _From, State) ->
    % Register a new handler for the agent
    NewHandlers = maps:put(AgentId, Handler, State#state.handlers),
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call({unregister_handler, AgentId}, _From, State) ->
    % Unregister a handler
    NewHandlers = maps:remove(AgentId, State#state.handlers),
    {reply, ok, State#state{handlers = NewHandlers}};

handle_call({send_message, AgentId, Message, Options}, From, State) ->
    % Send a message to a specific agent
    Result = case maps:find(AgentId, State#state.handlers) of
        {ok, Handler} ->
            deliver_message(Handler, Message, From, Options);
        error ->
            {error, {agent_not_found, AgentId}}
    end,
    
    % Update message history
    MessageRecord = #{
        timestamp => erlang:system_time(millisecond),
        type => direct,
        to => AgentId,
        message => Message,
        result => Result
    },
    NewHistory = update_history(State#state.message_history, MessageRecord),
    
    {reply, Result, State#state{message_history = NewHistory}};

handle_call({send_broadcast, Message, Options}, _From, State) ->
    % Send a message to all registered agents
    {SuccessCount, ErrorCount, Errors} = broadcast_message(State#state.handlers, Message, Options),
    
    % Update message history
    MessageRecord = #{
        timestamp => erlang:system_time(millisecond),
        type => broadcast,
        message => Message,
        success_count => SuccessCount,
        error_count => ErrorCount
    },
    NewHistory = update_history(State#state.message_history, MessageRecord),
    
    Result = case ErrorCount of
        0 -> {ok, SuccessCount};
        _ -> {error, {partial_delivery, SuccessCount, ErrorCount, Errors}}
    end,
    
    {reply, Result, State#state{message_history = NewHistory}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

%% Deliver a message to a handler (function or process)
deliver_message(Handler, Message, _From, Options) when is_function(Handler) ->
    try
        Response = Handler(Message, Options),
        {ok, Response}
    catch
        E:R:_S ->
            {error, {handler_error, E, R}}
    end;

deliver_message(Handler, Message, _From, Options) when is_pid(Handler) ->
    try
        % Generate a unique reference for this request
        Ref = make_ref(),
        Handler ! {agent_message, self(), Ref, Message, Options},
        
        % Wait for response with timeout
        Timeout = maps:get(timeout, Options, 5000),
        receive
            {agent_response, Ref, Response} ->
                {ok, Response}
        after Timeout ->
            {error, timeout}
        end
    catch
        E:R:_S ->
            {error, {delivery_error, E, R}}
    end;

deliver_message(_, _, _, _) ->
    {error, invalid_handler}.

%% Broadcast a message to all handlers
broadcast_message(Handlers, Message, Options) ->
    maps:fold(
        fun(AgentId, Handler, {SuccessAcc, ErrorAcc, ErrorsAcc}) ->
            case deliver_message(Handler, Message, undefined, Options) of
                {ok, _Response} ->
                    {SuccessAcc + 1, ErrorAcc, ErrorsAcc};
                {error, Reason} ->
                    {SuccessAcc, ErrorAcc + 1, [{AgentId, Reason} | ErrorsAcc]}
            end
        end,
        {0, 0, []},
        Handlers
    ).

%% Update message history, keeping only the most recent messages
update_history(History, NewMessage) ->
    % Add new message to the beginning of the list
    [NewMessage | History].