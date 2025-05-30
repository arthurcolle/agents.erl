-module(mcp_transport).
-behaviour(gen_server).

%% Transport abstraction for MCP protocol supporting:
%% - WebSocket (for remote MCP servers)
%% - stdio (for local MCP servers)
%% - HTTP/SSE (for advanced remote servers)

-export([start_link/3, send_message/2, close/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(PROTOCOL_VERSION, <<"2024-11-05">>).

-record(state, {
    transport_type, % websocket | stdio | http_sse
    transport_pid,
    stream_ref,
    connection_info = #{},
    owner_pid,
    buffer = <<>>, % For stdio line buffering
    message_handler
}).

%%====================================================================
%% API
%%====================================================================

start_link(TransportType, ConnectionInfo, MessageHandler) ->
    gen_server:start_link(?MODULE, {TransportType, ConnectionInfo, MessageHandler}, []).

send_message(Pid, Message) ->
    gen_server:call(Pid, {send_message, Message}).

close(Pid) ->
    gen_server:call(Pid, close).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init({TransportType, ConnectionInfo, MessageHandler}) ->
    process_flag(trap_exit, true),
    
    State = #state{
        transport_type = TransportType,
        connection_info = ConnectionInfo,
        owner_pid = self(),
        message_handler = MessageHandler
    },
    
    case start_transport(TransportType, ConnectionInfo) of
        {ok, TransportPid, StreamRef} ->
            {ok, State#state{
                transport_pid = TransportPid,
                stream_ref = StreamRef
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({send_message, Message}, _From, State) ->
    Result = send_transport_message(Message, State),
    {reply, Result, State};

handle_call(close, _From, State) ->
    cleanup_transport(State),
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% WebSocket transport messages
handle_info({gun_ws, Pid, StreamRef, {text, Data}}, 
            #state{transport_pid = Pid, stream_ref = StreamRef} = State) ->
    handle_message_data(Data, State);

handle_info({gun_ws, Pid, StreamRef, {close, Code, Reason}}, 
            #state{transport_pid = Pid, stream_ref = StreamRef} = State) ->
    notify_handler({connection_closed, {websocket, Code, Reason}}, State),
    {stop, {connection_closed, Code, Reason}, State};

handle_info({gun_error, Pid, StreamRef, Reason}, 
            #state{transport_pid = Pid, stream_ref = StreamRef} = State) ->
    notify_handler({transport_error, {websocket, Reason}}, State),
    {stop, {transport_error, Reason}, State};

%% stdio transport messages
handle_info({Port, {data, {eol, Line}}}, 
            #state{transport_type = stdio, transport_pid = Port} = State) ->
    handle_message_data(list_to_binary(Line), State);

handle_info({Port, {data, {noeol, Data}}}, 
            #state{transport_type = stdio, transport_pid = Port, buffer = Buffer} = State) ->
    NewBuffer = <<Buffer/binary, (list_to_binary(Data))/binary>>,
    {noreply, State#state{buffer = NewBuffer}};

handle_info({Port, {exit_status, Status}}, 
            #state{transport_type = stdio, transport_pid = Port} = State) ->
    notify_handler({connection_closed, {stdio, Status}}, State),
    {stop, {stdio_exit, Status}, State};

%% HTTP/SSE transport messages
handle_info({gun_sse, Pid, StreamRef, Data}, 
            #state{transport_type = http_sse, transport_pid = Pid, stream_ref = StreamRef} = State) ->
    case Data of
        #{data := MessageData} ->
            handle_message_data(MessageData, State);
        _ ->
            {noreply, State}
    end;

%% Process monitoring
handle_info({'EXIT', Pid, Reason}, 
            #state{transport_pid = Pid} = State) ->
    notify_handler({transport_died, Reason}, State),
    {stop, {transport_died, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cleanup_transport(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_transport(websocket, #{url := Url} = Info) ->
    start_websocket_transport(Url, Info);

start_transport(stdio, #{command := Command, args := Args} = Info) ->
    start_stdio_transport(Command, Args, Info);

start_transport(http_sse, #{url := Url} = Info) ->
    start_http_sse_transport(Url, Info);

start_transport(Type, _Info) ->
    {error, {unsupported_transport, Type}}.

start_websocket_transport(Url, _Info) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host, port := Port, path := Path} 
        when Scheme =:= <<"ws">> orelse Scheme =:= <<"wss">> ->
            
            Opts = case Scheme of
                <<"wss">> -> #{protocols => [http], transport => tls};
                <<"ws">> -> #{protocols => [http]}
            end,
            
            case gun:open(binary_to_list(Host), Port, Opts) of
                {ok, ConnPid} ->
                    case gun:await_up(ConnPid, 5000) of
                        {ok, _Protocol} ->
                            Headers = [
                                {<<"sec-websocket-protocol">>, <<"mcp">>}
                            ],
                            StreamRef = gun:ws_upgrade(ConnPid, binary_to_list(Path), Headers),
                            case gun:await(ConnPid, StreamRef, 5000) of
                                {upgrade, [<<"websocket">>], _Headers} ->
                                    {ok, ConnPid, StreamRef};
                                {response, _Fin, Status, _Headers} ->
                                    gun:close(ConnPid),
                                    {error, {http_error, Status}};
                                {error, Reason} ->
                                    gun:close(ConnPid),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, invalid_websocket_url}
    end.

start_stdio_transport(Command, Args, _Info) ->
    % Open port for stdio communication
    PortOpts = [
        {spawn_executable, Command},
        {args, Args},
        {line, 4096},
        stderr_to_stdout,
        exit_status,
        binary
    ],
    
    try
        Port = open_port({spawn_executable, Command}, PortOpts),
        {ok, Port, undefined}
    catch
        _:Reason ->
            {error, {stdio_open_failed, Reason}}
    end.

start_http_sse_transport(Url, _Info) ->
    case uri_string:parse(Url) of
        #{scheme := Scheme, host := Host, port := Port, path := Path} 
        when Scheme =:= <<"http">> orelse Scheme =:= <<"https">> ->
            
            Opts = case Scheme of
                <<"https">> -> #{transport => tls};
                <<"http">> -> #{}
            end,
            
            case gun:open(binary_to_list(Host), Port, Opts) of
                {ok, ConnPid} ->
                    case gun:await_up(ConnPid, 5000) of
                        {ok, _Protocol} ->
                            Headers = [
                                {<<"accept">>, <<"text/event-stream">>},
                                {<<"cache-control">>, <<"no-cache">>}
                            ],
                            StreamRef = gun:get(ConnPid, binary_to_list(Path), Headers),
                            case gun:await(ConnPid, StreamRef, 5000) of
                                {response, nofin, 200, _Headers} ->
                                    {ok, ConnPid, StreamRef};
                                {response, _Fin, Status, _Headers} ->
                                    gun:close(ConnPid),
                                    {error, {http_error, Status}};
                                {error, Reason} ->
                                    gun:close(ConnPid),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            gun:close(ConnPid),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, invalid_http_url}
    end.

send_transport_message(Message, #state{transport_type = websocket} = State) ->
    try
        Data = jsx:encode(Message),
        gun:ws_send(State#state.transport_pid, State#state.stream_ref, {text, Data}),
        ok
    catch
        _:Reason ->
            {error, {encode_error, Reason}}
    end;

send_transport_message(Message, #state{transport_type = stdio} = State) ->
    try
        Data = jsx:encode(Message),
        % Add newline for stdio transport
        MessageWithNewline = <<Data/binary, "\n">>,
        port_command(State#state.transport_pid, MessageWithNewline),
        ok
    catch
        _:Reason ->
            {error, {send_error, Reason}}
    end;

send_transport_message(Message, #state{transport_type = http_sse} = State) ->
    try
        Data = jsx:encode(Message),
        gun:post(State#state.transport_pid, "/mcp", [
            {<<"content-type">>, <<"application/json">>}
        ], Data),
        ok
    catch
        _:Reason ->
            {error, {send_error, Reason}}
    end.

handle_message_data(Data, State) ->
    try
        Message = jsx:decode(Data, [return_maps]),
        notify_handler({message_received, Message}, State),
        {noreply, State}
    catch
        _:_ ->
            error_logger:warning_msg("Invalid JSON received: ~p~n", [Data]),
            {noreply, State}
    end.

notify_handler(Event, #state{message_handler = Handler}) when is_function(Handler, 1) ->
    Handler(Event);

notify_handler(Event, #state{message_handler = {M, F}}) ->
    M:F(Event);

notify_handler(Event, #state{message_handler = Pid}) when is_pid(Pid) ->
    Pid ! Event;

notify_handler(_Event, _State) ->
    ok.

cleanup_transport(#state{transport_type = websocket, transport_pid = Pid}) ->
    try gun:close(Pid) catch _:_ -> ok end;

cleanup_transport(#state{transport_type = stdio, transport_pid = Port}) ->
    try port_close(Port) catch _:_ -> ok end;

cleanup_transport(#state{transport_type = http_sse, transport_pid = Pid}) ->
    try gun:close(Pid) catch _:_ -> ok end;

cleanup_transport(_State) ->
    ok.