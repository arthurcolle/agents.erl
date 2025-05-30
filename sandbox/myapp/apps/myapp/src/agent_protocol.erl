%% agent_protocol.erl
%% Protocol and message format specification for agent communication
-module(agent_protocol).

%% API for message creation
-export([
    create_message/3,
    create_request/3,
    create_response/4,
    create_broadcast/2,
    create_event/3
]).

%% API for message validation
-export([
    validate_message/1,
    is_request/1,
    is_response/1,
    is_broadcast/1,
    is_event/1
]).

%% API for message extraction
-export([
    get_sender/1,
    get_receiver/1,
    get_message_id/1,
    get_payload/1,
    get_type/1,
    get_correlation_id/1,
    get_timestamp/1
]).

%% API for encoding/decoding
-export([
    encode/1,
    decode/1,
    decode/2
]).

%% Message type constants
-define(TYPE_REQUEST, <<"request">>).
-define(TYPE_RESPONSE, <<"response">>).
-define(TYPE_BROADCAST, <<"broadcast">>).
-define(TYPE_EVENT, <<"event">>).
-define(TYPE_ERROR, <<"error">>).

%% API Functions

%% Create a generic message
-spec create_message(Type, Sender, Payload) -> Message when
    Type :: binary(),
    Sender :: binary() | atom(),
    Payload :: map(),
    Message :: map().
create_message(Type, Sender, Payload) ->
    #{
        type => Type,
        id => generate_id(),
        sender => ensure_binary(Sender),
        timestamp => erlang:system_time(millisecond),
        payload => Payload
    }.

%% Create a request message
-spec create_request(Sender, Receiver, Payload) -> Message when
    Sender :: binary() | atom(),
    Receiver :: binary() | atom(),
    Payload :: map(),
    Message :: map().
create_request(Sender, Receiver, Payload) ->
    (create_message(?TYPE_REQUEST, Sender, Payload))#{
        receiver => ensure_binary(Receiver)
    }.

%% Create a response message
-spec create_response(CorrelationId, Sender, Receiver, Payload) -> Message when
    CorrelationId :: binary(),
    Sender :: binary() | atom(),
    Receiver :: binary() | atom(),
    Payload :: map(),
    Message :: map().
create_response(CorrelationId, Sender, Receiver, Payload) ->
    (create_message(?TYPE_RESPONSE, Sender, Payload))#{
        receiver => ensure_binary(Receiver),
        correlation_id => CorrelationId
    }.

%% Create a broadcast message
-spec create_broadcast(Sender, Payload) -> Message when
    Sender :: binary() | atom(),
    Payload :: map(),
    Message :: map().
create_broadcast(Sender, Payload) ->
    create_message(?TYPE_BROADCAST, Sender, Payload).

%% Create an event message
-spec create_event(Sender, EventName, EventData) -> Message when
    Sender :: binary() | atom(),
    EventName :: binary() | atom(),
    EventData :: map(),
    Message :: map().
create_event(Sender, EventName, EventData) ->
    create_message(?TYPE_EVENT, Sender, #{
        event => ensure_binary(EventName),
        data => EventData
    }).

%% Validate a message
-spec validate_message(Message) -> Result when
    Message :: map(),
    Result :: ok | {error, Reason},
    Reason :: term().
validate_message(Message) when is_map(Message) ->
    % Required fields for all messages
    RequiredFields = [type, id, sender, timestamp, payload],
    
    % Check if all required fields are present
    case lists:all(fun(Field) -> maps:is_key(Field, Message) end, RequiredFields) of
        false ->
            {error, missing_required_fields};
        true ->
            % Type-specific validation
            Type = maps:get(type, Message),
            validate_by_type(Type, Message)
    end;
validate_message(_) ->
    {error, not_a_map}.

%% Check if a message is a request
-spec is_request(Message) -> boolean() when
    Message :: map().
is_request(Message) ->
    maps:get(type, Message, undefined) =:= ?TYPE_REQUEST.

%% Check if a message is a response
-spec is_response(Message) -> boolean() when
    Message :: map().
is_response(Message) ->
    maps:get(type, Message, undefined) =:= ?TYPE_RESPONSE.

%% Check if a message is a broadcast
-spec is_broadcast(Message) -> boolean() when
    Message :: map().
is_broadcast(Message) ->
    maps:get(type, Message, undefined) =:= ?TYPE_BROADCAST.

%% Check if a message is an event
-spec is_event(Message) -> boolean() when
    Message :: map().
is_event(Message) ->
    maps:get(type, Message, undefined) =:= ?TYPE_EVENT.

%% Extract sender from a message
-spec get_sender(Message) -> Sender when
    Message :: map(),
    Sender :: binary().
get_sender(Message) ->
    maps:get(sender, Message).

%% Extract receiver from a message
-spec get_receiver(Message) -> Receiver when
    Message :: map(),
    Receiver :: binary() | undefined.
get_receiver(Message) ->
    maps:get(receiver, Message, undefined).

%% Extract message ID
-spec get_message_id(Message) -> Id when
    Message :: map(),
    Id :: binary().
get_message_id(Message) ->
    maps:get(id, Message).

%% Extract payload
-spec get_payload(Message) -> Payload when
    Message :: map(),
    Payload :: map().
get_payload(Message) ->
    maps:get(payload, Message).

%% Extract message type
-spec get_type(Message) -> Type when
    Message :: map(),
    Type :: binary().
get_type(Message) ->
    maps:get(type, Message).

%% Extract correlation ID
-spec get_correlation_id(Message) -> CorrelationId when
    Message :: map(),
    CorrelationId :: binary() | undefined.
get_correlation_id(Message) ->
    maps:get(correlation_id, Message, undefined).

%% Extract timestamp
-spec get_timestamp(Message) -> Timestamp when
    Message :: map(),
    Timestamp :: integer().
get_timestamp(Message) ->
    maps:get(timestamp, Message).

%% Internal Functions

%% Generate a unique message ID
generate_id() ->
    erlang:list_to_binary(uuid:to_string(uuid:uuid4())).

%% Ensure a term is binary
ensure_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
ensure_binary(Term) when is_list(Term) ->
    list_to_binary(Term);
ensure_binary(Term) when is_binary(Term) ->
    Term;
ensure_binary(Term) ->
    term_to_binary(Term).

%% Validate a message based on its type
validate_by_type(?TYPE_REQUEST, Message) ->
    % Request must have a receiver
    case maps:is_key(receiver, Message) of
        true -> ok;
        false -> {error, missing_receiver}
    end;

validate_by_type(?TYPE_RESPONSE, Message) ->
    % Response must have a receiver and correlation_id
    case {maps:is_key(receiver, Message), maps:is_key(correlation_id, Message)} of
        {true, true} -> ok;
        {false, _} -> {error, missing_receiver};
        {_, false} -> {error, missing_correlation_id}
    end;

validate_by_type(?TYPE_BROADCAST, _Message) ->
    % Broadcasts only need the common fields
    ok;

validate_by_type(?TYPE_EVENT, Message) ->
    % Event must have a properly structured payload with event name and data
    Payload = maps:get(payload, Message, #{}),
    case {maps:is_key(event, Payload), maps:is_key(data, Payload)} of
        {true, true} -> ok;
        {false, _} -> {error, missing_event_name};
        {_, false} -> {error, missing_event_data}
    end;

validate_by_type(_, _) ->
    {error, invalid_message_type}.

%% Encoding/Decoding Functions

%% Encode a message to JSON binary
-spec encode(Message) -> Binary when
    Message :: map(),
    Binary :: binary().
encode(Message) when is_map(Message) ->
    try
        jsx:encode(Message)
    catch
        _:Reason ->
            error({encode_failed, Reason})
    end;
encode(_) ->
    error(badarg).

%% Decode a JSON binary to message
-spec decode(Binary) -> {ok, Message} | {error, Reason} when
    Binary :: binary(),
    Message :: map(),
    Reason :: term().
decode(Binary) ->
    decode(Binary, #{}).

%% Decode with options
-spec decode(Binary, Options) -> {ok, Message} | {error, Reason} when
    Binary :: binary(),
    Options :: map(),
    Message :: map(),
    Reason :: term().
decode(Binary, Options) when is_binary(Binary) ->
    try
        Message = jsx:decode(Binary, [return_maps]),
        % Validate if requested
        case maps:get(validate, Options, true) of
            true ->
                case validate_message(Message) of
                    ok -> {ok, Message};
                    Error -> Error
                end;
            false ->
                {ok, Message}
        end
    catch
        _:Reason ->
            {error, {decode_failed, Reason}}
    end;
decode(_, _) ->
    {error, not_binary}.