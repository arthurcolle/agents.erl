%%%-------------------------------------------------------------------
%%% @doc
%%% Conversation Storage Initializer
%%% Ensures conversation table is created at startup
%%% @end
%%%-------------------------------------------------------------------
-module(conversation_initializer).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(conversation, {
    id,
    name,
    agent_id,
    messages,
    created_at,
    updated_at
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    % Create the conversations table with persistence
    persistent_table_manager:ensure_ets_table(conversations, 
        [named_table, public], true),
    
    % Load conversations from disk if available
    load_conversations_from_disk(),
    
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_conversations_from_disk() ->
    FilePath = get_conversations_file_path(),
    case file:read_file(FilePath) of
        {ok, Data} ->
            try
                Conversations = jsx:decode(Data, [return_maps]),
                lists:foreach(fun(ConvMap) ->
                    Conversation = map_to_conversation(ConvMap),
                    try
                        ets:insert(conversations, {Conversation#conversation.id, Conversation})
                    catch
                        error:badarg ->
                            logger:warning("Could not insert conversation into ETS table: ~p", [Conversation#conversation.id])
                    end
                end, Conversations)
            catch
                _:Error ->
                    logger:error("Failed to load conversations from disk: ~p", [Error])
            end;
        {error, enoent} ->
            ok  % File doesn't exist yet
    end.

map_to_conversation(Map) ->
    #conversation{
        id = maps:get(<<"id">>, Map),
        name = maps:get(<<"name">>, Map, <<"Untitled Conversation">>),
        agent_id = maps:get(<<"agent_id">>, Map, null),
        messages = maps:get(<<"messages">>, Map, []),
        created_at = maps:get(<<"created_at">>, Map, erlang:system_time(millisecond)),
        updated_at = maps:get(<<"updated_at">>, Map, erlang:system_time(millisecond))
    }.

get_conversations_file_path() ->
    PrivDir = case code:priv_dir(agent_web) of
        {error, _} -> "priv";
        Dir -> Dir
    end,
    filename:join([PrivDir, "data", "conversations.json"]).