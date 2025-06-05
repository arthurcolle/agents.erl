%%%-------------------------------------------------------------------
%%% @doc Simple test worker for dynamic supervisor testing
%%% @end
%%%-------------------------------------------------------------------
-module(test_worker).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, ping/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    name :: atom(),
    started_at :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(test_worker).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

ping(Worker) ->
    gen_server:call(Worker, ping).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Name]) ->
    io:format("[TEST_WORKER] ~p starting up~n", [Name]),
    {ok, #state{name = Name, started_at = erlang:system_time(second)}}.

handle_call(ping, _From, State) ->
    Uptime = erlang:system_time(second) - State#state.started_at,
    {reply, {pong, State#state.name, uptime, Uptime}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    io:format("[TEST_WORKER] ~p shutting down~n", [State#state.name]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.