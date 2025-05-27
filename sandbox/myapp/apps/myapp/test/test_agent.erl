%%%-------------------------------------------------------------------
%%% @doc
%%% Test Agent Module
%%% Simple gen_server for testing agent functionality
%%% @end
%%%-------------------------------------------------------------------
-module(test_agent).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

start_link(Id) ->
    gen_server:start_link(?MODULE, [Id], []).

stop(Pid) ->
    gen_server:stop(Pid).

init([Id]) ->
    {ok, #{id => Id}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.