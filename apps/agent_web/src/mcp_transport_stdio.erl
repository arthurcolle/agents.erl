-module(mcp_transport_stdio).
-behaviour(gen_server).

%% API
-export([connect/2,
         send/2,
         disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    port,
    client_pid,
    buffer = <<>>,
    decoder_state
}).

%% API Functions
connect(Command, Args) ->
    gen_server:start_link(?MODULE, {Command, Args, self()}, []).

send(Pid, Message) ->
    gen_server:call(Pid, {send, Message}).

disconnect(Pid) ->
    gen_server:stop(Pid).

%% gen_server callbacks
init({Command, Args, ClientPid}) ->
    process_flag(trap_exit, true),
    
    % Build port command
    Executable = case Command of
        <<"npx">> -> "npx";
        <<"node">> -> "node";
        <<"python">> -> "python";
        <<"python3">> -> "python3";
        Cmd when is_binary(Cmd) -> binary_to_list(Cmd);
        Cmd when is_list(Cmd) -> Cmd
    end,
    
    % Convert args to strings
    ArgStrings = [binary_to_list(Arg) || Arg <- Args, is_binary(Arg)],
    
    % Build full command
    PortCommand = case ArgStrings of
        [] -> Executable;
        _ -> Executable ++ " " ++ string:join(ArgStrings, " ")
    end,
    
    io:format("[STDIO] Starting command: ~s~n", [PortCommand]),
    
    % Open port with proper options
    PortOptions = [
        {spawn, PortCommand},
        binary,
        use_stdio,
        exit_status,
        {line, 65536},
        {env, [{"MCP_ENV", "production"}]}
    ],
    
    try
        Port = open_port({spawn, PortCommand}, PortOptions),
        
        State = #state{
            port = Port,
            client_pid = ClientPid,
            decoder_state = mcp_json_decoder:init()
        },
        
        {ok, State}
    catch
        error:Reason ->
            io:format("[STDIO] Failed to start command: ~p~n", [Reason]),
            {stop, {error, {spawn_failed, Reason}}}
    end.

handle_call({send, Message}, _From, State) ->
    JsonData = jsx:encode(Message),
    JsonLine = <<JsonData/binary, "\n">>,
    
    try
        port_command(State#state.port, JsonLine),
        {reply, ok, State}
    catch
        error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Port, {data, {eol, Line}}}, State) when Port =:= State#state.port ->
    % Handle complete line
    NewState = process_line(Line, State),
    {noreply, NewState};

handle_info({Port, {data, {noeol, Chunk}}}, State) when Port =:= State#state.port ->
    % Accumulate partial data
    NewBuffer = <<(State#state.buffer)/binary, Chunk/binary>>,
    {noreply, State#state{buffer = NewBuffer}};

handle_info({Port, {exit_status, Status}}, State) when Port =:= State#state.port ->
    io:format("[STDIO] Process exited with status: ~p~n", [Status]),
    State#state.client_pid ! {transport_closed, {exit_status, Status}},
    {stop, normal, State};

handle_info({'EXIT', Port, Reason}, State) when Port =:= State#state.port ->
    io:format("[STDIO] Port terminated: ~p~n", [Reason]),
    State#state.client_pid ! {transport_closed, Reason},
    {stop, normal, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch port_close(State#state.port),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
process_line(Line, State) ->
    % Combine with any buffered data
    Data = case State#state.buffer of
        <<>> -> Line;
        Buffer -> <<Buffer/binary, Line/binary>>
    end,
    
    % Try to decode JSON
    case jsx:is_json(Data) of
        true ->
            try
                Message = jsx:decode(Data, [return_maps]),
                State#state.client_pid ! {transport_message, Message},
                State#state{buffer = <<>>}
            catch
                error:Reason ->
                    io:format("[STDIO] JSON decode error: ~p~nData: ~s~n", 
                             [Reason, Data]),
                    State#state{buffer = <<>>}
            end;
        false ->
            % Not valid JSON, might be stderr or other output
            io:format("[STDIO] Non-JSON output: ~s~n", [Data]),
            State#state{buffer = <<>>}
    end.