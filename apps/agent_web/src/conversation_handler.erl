-module(conversation_handler).
-export([init/2, allowed_methods/2, content_types_accepted/2, content_types_provided/2,
         handle_get/2, handle_post/2, handle_delete/2, post_is_create/2, create_path/2, from_json/2,
         init_storage/0]).

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

% Add a function to handle POST correctly in cowboy_rest
post_is_create(Req, State) ->
    {true, Req, State}.

create_path(Req, State) ->
    % Generate path for created resource
    ConversationId = generate_conversation_id(),
    Path = <<"/api/conversations/", ConversationId/binary>>,
    {Path, Req, State}.

from_json(Req, State) ->
    handle_post(Req, State).

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
        % Check if this is an update (ID provided) or create new
        ConversationId = case maps:get(<<"id">>, Data, undefined) of
            undefined -> generate_conversation_id();
            ExistingId -> ExistingId
        end,
        
        % Check if conversation exists
        {IsUpdate, CreatedAt} = case lookup_conversation(ConversationId) of
            {ok, ExistingConv} -> 
                {true, ExistingConv#conversation.created_at};
            _ -> 
                {false, erlang:system_time(millisecond)}
        end,
        
        Conversation = #conversation{
            id = ConversationId,
            name = maps:get(<<"name">>, Data, <<"Untitled Conversation">>),
            agent_id = maps:get(<<"agent_id">>, Data, null),
            messages = maps:get(<<"messages">>, Data, []),
            created_at = CreatedAt,
            updated_at = erlang:system_time(millisecond)
        },
        
        % Store conversation (in memory for now, could be persistent storage)
        case store_conversation(Conversation) of
            ok -> 
                % Log conversation creation to stats logger only for new conversations
                if
                    not IsUpdate ->
                        try
                            conversation_stats_logger:log_conversation_start(ConversationId, 
                                maps:get(<<"agent_id">>, Data, <<"unknown">>))
                        catch
                            _:_ -> 
                                % Ignore stats logging errors
                                ok
                        end;
                    true ->
                        ok
                end,
                
                Response = #{
                    <<"id">> => ConversationId,
                    <<"status">> => if IsUpdate -> <<"updated">>; true -> <<"created">> end
                },
                
                Body2 = jsx:encode(Response),
                Req3 = cowboy_req:set_resp_body(Body2, Req2),
                Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
                {true, Req4, State};
            {error, not_ready} ->
                % Return proper error response instead of throwing
                ErrorBody = jsx:encode(#{<<"error">> => <<"Storage not ready">>}),
                Req3 = cowboy_req:set_resp_body(ErrorBody, Req2),
                Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),
                Req5 = cowboy_req:reply(503, Req4),
                {stop, Req5, State}
        end
    catch
        _:Error ->
            logger:error("Error saving conversation: ~p", [Error]),
            ErrorBody2 = jsx:encode(#{<<"error">> => <<"Invalid request">>}),
            ReqError1 = cowboy_req:set_resp_body(ErrorBody2, Req2),
            ReqError2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, ReqError1),
            ReqError3 = cowboy_req:reply(400, ReqError2),
            {stop, ReqError3, State}
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
                    ErrorBody = jsx:encode(#{<<"error">> => <<"Conversation not found">>}),
                    Req2 = cowboy_req:set_resp_body(ErrorBody, Req),
                    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                    Req4 = cowboy_req:reply(404, Req3),
                    {stop, Req4, State};
                {error, not_ready} ->
                    ErrorBody = jsx:encode(#{<<"error">> => <<"Storage not ready">>}),
                    Req2 = cowboy_req:set_resp_body(ErrorBody, Req),
                    Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
                    Req4 = cowboy_req:reply(503, Req3),
                    {stop, Req4, State}
            end
    end.

list_conversations(Req, State) ->
    % Parse pagination parameters
    PaginationParams = pagination_utils:parse_pagination_params(Req),
    #{page := _Page, page_size := _PageSize, offset := Offset} = PaginationParams,
    
    % Get all conversations
    AllConversations = get_all_conversations(),
    
    % Apply pagination
    {PageConversations, Metadata} = pagination_utils:paginate_list(
        AllConversations, 
        Offset, 
        maps:get(page_size, PaginationParams)
    ),
    
    % Format response with pagination metadata
    Response = pagination_utils:format_pagination_response(
        PageConversations, 
        Metadata, 
        <<"conversations">>, 
        #{}
    ),
    
    try
        Body = jsx:encode(Response),
        Req2 = cowboy_req:set_resp_body(Body, Req),
        Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
        {Body, Req3, State}
    catch
        error:Error ->
            logger:error("Error encoding conversation response: ~p", [Error]),
            ErrorBody = jsx:encode(#{<<"error">> => <<"Internal server error">>}),
            Req4 = cowboy_req:set_resp_body(ErrorBody, Req),
            Req5 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req4),
            {ErrorBody, Req5, State}
    end.

get_conversation(ConversationId, Req, State) ->
    case lookup_conversation(ConversationId) of
        {ok, Conversation} ->
            ConversationMap = conversation_to_map(Conversation),
            Body = jsx:encode(ConversationMap),
            Req2 = cowboy_req:set_resp_body(Body, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            {Body, Req3, State};
        {error, not_found} ->
            ErrorBody = jsx:encode(#{<<"error">> => <<"Conversation not found">>}),
            Req2 = cowboy_req:set_resp_body(ErrorBody, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            Req4 = cowboy_req:reply(404, Req3),
            {stop, Req4, State};
        {error, not_ready} ->
            ErrorBody = jsx:encode(#{<<"error">> => <<"Storage not ready">>}),
            Req2 = cowboy_req:set_resp_body(ErrorBody, Req),
            Req3 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req2),
            Req4 = cowboy_req:reply(503, Req3),
            {stop, Req4, State}
    end.

% Storage functions
store_conversation(Conversation) ->
    case is_table_ready() of
        false -> {error, not_ready};
        true ->
            try
                % Store in ETS
                ets:insert(conversations, {Conversation#conversation.id, Conversation}),
                % Persist to disk
                persist_conversation_to_disk(Conversation),
                ok
            catch
                error:badarg -> {error, not_ready}
            end
    end.

lookup_conversation(ConversationId) ->
    case is_table_ready() of
        false -> {error, not_ready};
        true ->
            try
                case ets:lookup(conversations, ConversationId) of
                    [{_, Conversation}] ->
                        {ok, Conversation};
                    [] ->
                        {error, not_found}
                end
            catch
                error:badarg -> {error, not_ready}
            end
    end.

delete_conversation(ConversationId) ->
    case is_table_ready() of
        false -> {error, not_ready};
        true ->
            try
                case ets:lookup(conversations, ConversationId) of
                    [{_, _}] ->
                        ets:delete(conversations, ConversationId),
                        ok;
                    [] ->
                        {error, not_found}
                end
            catch
                error:badarg -> {error, not_ready}
            end
    end.

get_all_conversations() ->
    case is_table_ready() of
        false -> [];
        true ->
            try
                Conversations = ets:tab2list(conversations),
                [conversation_to_map(Conv) || {_, Conv} <- Conversations]
            catch
                error:badarg -> []
            end
    end.

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

%% Initialize storage
init_storage() ->
    case ets:info(conversations) of
        undefined ->
            ets:new(conversations, [named_table, public, {keypos, 1}]),
            % Load persisted conversations
            load_conversations_from_disk();
        _ ->
            ok
    end.

%% Check if ETS table is ready
is_table_ready() ->
    try
        ets:info(conversations, name) =:= conversations
    catch
        error:badarg -> false
    end.