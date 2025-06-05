%% openai_background.erl
%% Background mode manager for long-running OpenAI tasks
-module(openai_background).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    create_background_response/3,
    create_background_response/4,
    poll_response/1,
    poll_response/2,
    cancel_response/1,
    stream_background_response/1,
    stream_background_response/2,
    get_active_responses/0,
    cleanup_completed_responses/0
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
-define(POLL_INTERVAL, 2000).  % Poll every 2 seconds
-define(CLEANUP_INTERVAL, 300000).  % Cleanup every 5 minutes

%% Logging macros
-define(LOG_INFO(Msg), colored_logger:data(processed, Msg)).
-define(LOG_INFO(Msg, Args), colored_logger:data(processed, io_lib:format(Msg, Args))).
-define(LOG_ERROR(Msg), colored_logger:fire(inferno, Msg)).
-define(LOG_ERROR(Msg, Args), colored_logger:fire(inferno, io_lib:format(Msg, Args))).
-define(LOG_WARNING(Msg), colored_logger:alarm(medium, Msg)).
-define(LOG_WARNING(Msg, Args), colored_logger:alarm(medium, io_lib:format(Msg, Args))).
-define(LOG_SUCCESS(Msg), colored_logger:complete(success, Msg)).
-define(LOG_SUCCESS(Msg, Args), colored_logger:complete(success, io_lib:format(Msg, Args))).

-record(state, {
    active_responses = #{} :: map(),  % ResponseId -> {Status, CreatedAt, LastChecked}
    polling_pids = #{} :: map(),      % ResponseId -> PollingPid
    cleanup_timer :: reference() | undefined
}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Create a background response
create_background_response(Input, Model, Options) ->
    create_background_response(Input, Model, Options, undefined).

create_background_response(Input, Model, Options, CallbackPid) ->
    ?LOG_INFO("[BACKGROUND] 🚀 Creating background response - Model: ~s", [Model]),
    
    % Enable background mode and ensure store=true (required for background)
    BackgroundOptions = Options#{
        background => true,
        store => true  % Required for background mode
    },
    
    case openai_responses:create_response(Input, Model, BackgroundOptions) of
        {ok, #{<<"id">> := ResponseId, <<"status">> := Status} = Response} ->
            ?LOG_SUCCESS("[BACKGROUND] ✅ Background response created: ~s (status: ~s)", [ResponseId, Status]),
            
            % Register the response for tracking
            gen_server:cast(?SERVER, {register_response, ResponseId, Status, CallbackPid}),
            
            % Start polling if not in terminal state
            case is_terminal_status(Status) of
                false ->
                    gen_server:cast(?SERVER, {start_polling, ResponseId});
                true ->
                    ?LOG_INFO("[BACKGROUND] Response ~s already in terminal state: ~s", [ResponseId, Status])
            end,
            
            {ok, Response};
        {error, Reason} ->
            ?LOG_ERROR("[BACKGROUND] ❌ Failed to create background response: ~p", [Reason]),
            {error, Reason}
    end.

%% Poll a specific response
poll_response(ResponseId) ->
    poll_response(ResponseId, #{}).

poll_response(ResponseId, Options) ->
    ?LOG_INFO("[BACKGROUND] 🔄 Polling response: ~s", [ResponseId]),
    
    case openai_responses:get_response(ResponseId, Options) of
        {ok, #{<<"status">> := Status} = Response} ->
            ?LOG_INFO("[BACKGROUND] 📊 Response ~s status: ~s", [ResponseId, Status]),
            
            % Update tracking
            gen_server:cast(?SERVER, {update_response_status, ResponseId, Status}),
            
            {ok, Response};
        {error, Reason} ->
            ?LOG_ERROR("[BACKGROUND] ❌ Failed to poll response ~s: ~p", [ResponseId, Reason]),
            {error, Reason}
    end.

%% Cancel a response
cancel_response(ResponseId) ->
    ?LOG_INFO("[BACKGROUND] 🛑 Cancelling response: ~s", [ResponseId]),
    
    case openai_responses:cancel_response(ResponseId) of
        {ok, #{<<"status">> := Status} = Response} ->
            ?LOG_SUCCESS("[BACKGROUND] ✅ Response ~s cancelled (status: ~s)", [ResponseId, Status]),
            
            % Update tracking and stop polling
            gen_server:cast(?SERVER, {update_response_status, ResponseId, Status}),
            gen_server:cast(?SERVER, {stop_polling, ResponseId}),
            
            {ok, Response};
        {error, Reason} ->
            ?LOG_ERROR("[BACKGROUND] ❌ Failed to cancel response ~s: ~p", [ResponseId, Reason]),
            {error, Reason}
    end.

%% Stream a background response
stream_background_response(ResponseId) ->
    stream_background_response(ResponseId, #{}).

stream_background_response(ResponseId, Options) ->
    ?LOG_INFO("[BACKGROUND] 🌊 Starting stream for background response: ~s", [ResponseId]),
    
    % Create streaming options
    StreamOptions = Options#{stream => true},
    
    % Note: The actual streaming implementation would require polling the stream endpoint
    % For now, we'll simulate by polling and checking for updates
    CallerPid = self(),
    StreamPid = spawn(fun() ->
        stream_background_loop(ResponseId, StreamOptions, CallerPid)
    end),
    
    {ok, StreamPid}.

%% Get all active responses
get_active_responses() ->
    gen_server:call(?SERVER, get_active_responses).

%% Cleanup completed responses
cleanup_completed_responses() ->
    gen_server:cast(?SERVER, cleanup_completed).

%% gen_server callbacks

init([]) ->
    ?LOG_INFO("[BACKGROUND] 🎬 Background manager starting..."),
    
    % Start cleanup timer
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_timer),
    
    {ok, #state{cleanup_timer = CleanupTimer}}.

handle_call(get_active_responses, _From, #state{active_responses = Responses} = State) ->
    {reply, {ok, Responses}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({register_response, ResponseId, Status, CallbackPid}, #state{active_responses = Responses} = State) ->
    Now = os:system_time(millisecond),
    ResponseInfo = #{
        status => Status,
        created_at => Now,
        last_checked => Now,
        callback_pid => CallbackPid
    },
    NewResponses = Responses#{ResponseId => ResponseInfo},
    ?LOG_INFO("[BACKGROUND] 📝 Registered response: ~s", [ResponseId]),
    {noreply, State#state{active_responses = NewResponses}};

handle_cast({update_response_status, ResponseId, Status}, #state{active_responses = Responses} = State) ->
    case maps:get(ResponseId, Responses, undefined) of
        undefined ->
            ?LOG_WARNING("[BACKGROUND] ⚠️  Trying to update unknown response: ~s", [ResponseId]),
            {noreply, State};
        ResponseInfo ->
            Now = os:system_time(millisecond),
            UpdatedInfo = ResponseInfo#{status => Status, last_checked => Now},
            NewResponses = Responses#{ResponseId => UpdatedInfo},
            
            % Notify callback if provided
            case maps:get(callback_pid, ResponseInfo, undefined) of
                undefined -> ok;
                CallbackPid when is_pid(CallbackPid) ->
                    CallbackPid ! {background_response_update, ResponseId, Status}
            end,
            
            {noreply, State#state{active_responses = NewResponses}}
    end;

handle_cast({start_polling, ResponseId}, #state{polling_pids = PollingPids} = State) ->
    case maps:get(ResponseId, PollingPids, undefined) of
        undefined ->
            PollingPid = spawn(fun() -> polling_loop(ResponseId) end),
            NewPollingPids = PollingPids#{ResponseId => PollingPid},
            ?LOG_INFO("[BACKGROUND] 🔄 Started polling for response: ~s", [ResponseId]),
            {noreply, State#state{polling_pids = NewPollingPids}};
        _ExistingPid ->
            ?LOG_WARNING("[BACKGROUND] ⚠️  Polling already active for response: ~s", [ResponseId]),
            {noreply, State}
    end;

handle_cast({stop_polling, ResponseId}, #state{polling_pids = PollingPids} = State) ->
    case maps:get(ResponseId, PollingPids, undefined) of
        undefined ->
            {noreply, State};
        PollingPid ->
            exit(PollingPid, shutdown),
            NewPollingPids = maps:remove(ResponseId, PollingPids),
            ?LOG_INFO("[BACKGROUND] 🛑 Stopped polling for response: ~s", [ResponseId]),
            {noreply, State#state{polling_pids = NewPollingPids}}
    end;

handle_cast(cleanup_completed, #state{active_responses = Responses, polling_pids = PollingPids} = State) ->
    ?LOG_INFO("[BACKGROUND] 🧹 Cleaning up completed responses..."),
    
    % Find terminal responses older than 1 hour
    OneHourAgo = os:system_time(millisecond) - (60 * 60 * 1000),
    
    {ToRemove, ToKeep} = maps:fold(fun(ResponseId, ResponseInfo, {Remove, Keep}) ->
        Status = maps:get(status, ResponseInfo, <<"unknown">>),
        CreatedAt = maps:get(created_at, ResponseInfo, 0),
        
        case is_terminal_status(Status) andalso CreatedAt < OneHourAgo of
            true -> {[ResponseId | Remove], Keep};
            false -> {Remove, Keep#{ResponseId => ResponseInfo}}
        end
    end, {[], #{}}, Responses),
    
    % Stop polling for removed responses
    lists:foreach(fun(ResponseId) ->
        case maps:get(ResponseId, PollingPids, undefined) of
            undefined -> ok;
            PollingPid -> exit(PollingPid, shutdown)
        end
    end, ToRemove),
    
    NewPollingPids = maps:without(ToRemove, PollingPids),
    
    ?LOG_INFO("[BACKGROUND] 🗑️  Cleaned up ~p completed responses", [length(ToRemove)]),
    {noreply, State#state{active_responses = ToKeep, polling_pids = NewPollingPids}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_timer, State) ->
    % Trigger cleanup
    gen_server:cast(?SERVER, cleanup_completed),
    
    % Restart timer
    CleanupTimer = erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_timer),
    {noreply, State#state{cleanup_timer = CleanupTimer}};

handle_info({polling_completed, ResponseId, Status}, State) ->
    % Polling completed for a response
    gen_server:cast(?SERVER, {update_response_status, ResponseId, Status}),
    gen_server:cast(?SERVER, {stop_polling, ResponseId}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{cleanup_timer = Timer, polling_pids = PollingPids}) ->
    % Cancel cleanup timer
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,
    
    % Stop all polling processes
    maps:foreach(fun(_, PollingPid) ->
        exit(PollingPid, shutdown)
    end, PollingPids),
    
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

is_terminal_status(<<"completed">>) -> true;
is_terminal_status(<<"failed">>) -> true;
is_terminal_status(<<"cancelled">>) -> true;
is_terminal_status(_) -> false.

polling_loop(ResponseId) ->
    ?LOG_INFO("[BACKGROUND] 🔄 Polling response: ~s", [ResponseId]),
    
    case openai_responses:get_response(ResponseId, #{}) of
        {ok, #{<<"status">> := Status}} ->
            % Update the main process
            gen_server:cast(?SERVER, {update_response_status, ResponseId, Status}),
            
            case is_terminal_status(Status) of
                true ->
                    ?LOG_SUCCESS("[BACKGROUND] 🏁 Response ~s completed with status: ~s", [ResponseId, Status]),
                    openai_background ! {polling_completed, ResponseId, Status};
                false ->
                    ?LOG_INFO("[BACKGROUND] 🔄 Response ~s still in progress: ~s", [ResponseId, Status]),
                    timer:sleep(?POLL_INTERVAL),
                    polling_loop(ResponseId)
            end;
        {error, Reason} ->
            ?LOG_ERROR("[BACKGROUND] ❌ Polling failed for response ~s: ~p", [ResponseId, Reason]),
            timer:sleep(?POLL_INTERVAL * 2),  % Wait longer on error
            polling_loop(ResponseId)
    end.

stream_background_loop(ResponseId, _Options, CallerPid) ->
    % For background streaming, we would need to implement cursor-based streaming
    % This is a simplified version that polls for updates
    ?LOG_INFO("[BACKGROUND] 🌊 Background streaming not fully implemented - polling instead"),
    
    case openai_responses:get_response(ResponseId, #{}) of
        {ok, #{<<"status">> := Status, <<"output_text">> := OutputText}} when Status =:= <<"completed">> ->
            % Send the complete text as a stream event
            CallerPid ! {stream_event, #{<<"type">> => <<"response.output_text.done">>, <<"output_text">> => OutputText}},
            CallerPid ! stream_complete;
        {ok, #{<<"status">> := Status}} ->
            CallerPid ! {stream_event, #{<<"type">> => <<"response.in_progress">>, <<"status">> => Status}},
            timer:sleep(?POLL_INTERVAL),
            stream_background_loop(ResponseId, _Options, CallerPid);
        {error, Reason} ->
            CallerPid ! {stream_error, Reason}
    end.