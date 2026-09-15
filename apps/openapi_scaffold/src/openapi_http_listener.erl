%%%-------------------------------------------------------------------
%%% @doc OTP owner for the OpenAPI Cowboy listener.
%%%
%%% Cowboy/Ranch has its own supervision tree. This process provides the
%%% application with one explicit owner whose terminate callback stops that
%%% tree cleanly during normal shutdown and supervisor replacement.
%%% @end
%%%-------------------------------------------------------------------
-module(openapi_http_listener).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(LISTENER_REF, openapi_http_listener).

-record(state, {
    listener_pid :: pid()
}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:stop(?MODULE)
    end.

init([]) ->
    case openapi_scaffold_app:start_http_server() of
        {ok, ListenerPid} when is_pid(ListenerPid) ->
            {ok, #state{listener_pid = ListenerPid}};
        {error, Reason} ->
            {stop, {listener_start_failed, Reason}};
        Other ->
            {stop, {listener_start_failed, Other}}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unsupported}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({'EXIT', ListenerPid, Reason}, #state{listener_pid = ListenerPid} = State) ->
    {stop, {listener_died, Reason}, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    try cowboy:stop_listener(?LISTENER_REF) of
        _ -> ok
    catch
        _:_ -> ok
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
