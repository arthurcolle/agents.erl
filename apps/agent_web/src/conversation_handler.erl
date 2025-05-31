-module(conversation_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,
         handle_get/2, handle_post/2, handle_delete/2, init_storage/0]).

-record(conversation, {
    id,
    name,
    agent_id,
    messages,
    created_at,
    updated_at
}).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>, <<"DELETE">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get}], Req, State}.

content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post}], Req, State}.

handle_get(Req, State) ->
    Method = cowboy_req:method(Req),
    case Method of
        <<"GET">> ->
            ConversationId = cowboy_req:binding(conversation_id, Req),
            case ConversationId of
                undefined ->
                    % List all conversations
                    list_conversations(Req, State);
                _ ->
                    % Get specific conversation
                    get_conversation(ConversationId, Req, State)
            end;
        _ ->
            {false, Req, State}
    end.

handle_post(Req, State) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    try
        Data = jsx:decode(Body, [return_maps]),
        ConversationId = generate_conversation_id(),
        Conversation = #conversation{
            id = ConversationId,
            name = maps:get(<<"name">>, Data, <<"Untitled Conversation">>),
            agent_id = maps:get(<<"agent_id">>, Data, null),
            messages = maps:get(<<"messages">>, Data, []),
            created_at = erlang:system_time(millisecond),
            updated_at = erlang:system_time(millisecond)
        },
        
        % Store conversation (in memory for now, could be persistent storage)
        ok = store_conversation(Conversation),
        
        Response = #{
            <<"id">> => ConversationId,
            <<"status">> => <<"saved">>
        },
        
        Body2 = jsx:encode(Response),
        Req3 = cowboy_req:set_resp_body(Body2, Req2),
        Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
        {true, Req4, State}
    catch
        _:Error ->
            logger:error("Error saving conversation: ~p", [Error]),
            Req5 = cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, 
                                   jsx:encode(#{<<"error">> => <<"Invalid request">>}), Req2),
            {false, Req5, State}
    end.

handle_delete(Req, State) ->
    ConversationId = cowboy_req:binding(conversation_id, Req),
    case ConversationId of
        undefined ->
            {false, Req, State};
        _ ->
            case delete_conversation(ConversationId) of
                ok ->
                    Response = #{<<"status">> => <<"deleted">>},
                    Body = jsx:encode(Response),
                    Req2 = cowboy_req:set_resp_body(Body, Req),
                    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                    {true, Req3, State};
                {error, not_found} ->
                    Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                                           jsx:encode(#{<<"error">> => <<"Conversation not found">>}), Req),
                    {false, Req2, State}
            end
    end.

list_conversations(Req, State) ->
    Conversations = get_all_conversations(),
    Body = jsx:encode(#{<<"conversations">> => Conversations}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
    {Body, Req3, State}.

get_conversation(ConversationId, Req, State) ->
    case lookup_conversation(ConversationId) of
        {ok, Conversation} ->
            ConversationMap = conversation_to_map(Conversation),
            Body = jsx:encode(ConversationMap),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            {Body, Req3, State};
        {error, not_found} ->
            Req2 = cowboy_req:reply(404, #{<<"content-type">> => <<"application/json">>}, 
                                   jsx:encode(#{<<"error">> => <<"Conversation not found">>}), Req),
            {false, Req2, State}
    end.

% Storage functions
store_conversation(Conversation) ->
    % Ensure storage is initialized before accessing
    init_storage(),
    % Store in ETS
    ets:insert(conversations, {Conversation#conversation.id, Conversation}),
    % Persist to disk
    persist_conversation_to_disk(Conversation),
    ok.

lookup_conversation(ConversationId) ->
    % Ensure storage is initialized before accessing
    init_storage(),
    case ets:lookup(conversations, ConversationId) of
        [{_, Conversation}] ->
            {ok, Conversation};
        [] ->
            {error, not_found}
    end.

delete_conversation(ConversationId) ->
    % Ensure storage is initialized before accessing
    init_storage(),
    case ets:lookup(conversations, ConversationId) of
        [{_, _}] ->
            ets:delete(conversations, ConversationId),
            ok;
        [] ->
            {error, not_found}
    end.

get_all_conversations() ->
    % Ensure storage is initialized before accessing
    init_storage(),
    Conversations = ets:tab2list(conversations),
    [conversation_to_map(Conv) || {_, Conv} <- Conversations].

conversation_to_map(#conversation{id = Id, name = Name, agent_id = AgentId, 
                                  messages = Messages, created_at = CreatedAt, 
                                  updated_at = UpdatedAt}) ->
    #{
        <<"id">> => Id,
        <<"name">> => Name,
        <<"agent_id">> => AgentId,
        <<"messages">> => Messages,
        <<"created_at">> => CreatedAt,
        <<"updated_at">> => UpdatedAt
    }.

generate_conversation_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

% Initialize ETS table on module load
init_storage() ->
    case ets:info(conversations) of
        undefined ->
            ets:new(conversations, [named_table, public]),
            % Load persisted conversations
            load_conversations_from_disk();
        _ ->
            ok
    end.

% Disk persistence functions
persist_conversation_to_disk(Conversation) ->
    FilePath = get_conversations_file_path(),
    ConversationMap = conversation_to_map(Conversation),
    _ConversationJson = jsx:encode(ConversationMap),
    % Create directory if it doesn't exist
    filelib:ensure_dir(FilePath),
    % Write to temporary file first, then rename (atomic operation)
    TempFile = FilePath ++ ".tmp",
    AllConversations = get_all_conversations_as_json(),
    file:write_file(TempFile, jsx:encode(AllConversations)),
    file:rename(TempFile, FilePath).

load_conversations_from_disk() ->
    FilePath = get_conversations_file_path(),
    case file:read_file(FilePath) of
        {ok, Data} ->
            try
                Conversations = jsx:decode(Data, [return_maps]),
                lists:foreach(fun(ConvMap) ->
                    Conversation = map_to_conversation(ConvMap),
                    ets:insert(conversations, {Conversation#conversation.id, Conversation})
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

get_all_conversations_as_json() ->
    Conversations = get_all_conversations(),
    Conversations.