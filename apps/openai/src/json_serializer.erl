%% json_serializer.erl
%% JSON serializer wrapper that ensures jiffy is used for streaming tokens
%% Provides a consistent interface for JSON operations across the system

-module(json_serializer).

-export([
    encode/1,
    encode/2,
    decode/1,
    decode/2,
    encode_streaming_token/1,
    encode_streaming_response/1,
    decode_streaming_chunk/1,
    is_json/1
]).

%% Default options for jiffy
-define(DEFAULT_ENCODE_OPTIONS, [
    {escape_forward_slashes, false},
    {use_nil, false}
]).

-define(DEFAULT_DECODE_OPTIONS, [
    return_maps
]).

%% Encode data to JSON using jiffy
encode(Data) ->
    encode(Data, ?DEFAULT_ENCODE_OPTIONS).

encode(Data, Options) ->
    try
        jiffy:encode(Data, Options)
    catch
        error:Reason ->
            {error, {json_encode_error, Reason}}
    end.

%% Decode JSON using jiffy
decode(Json) ->
    decode(Json, ?DEFAULT_DECODE_OPTIONS).

decode(Json, Options) ->
    try
        jiffy:decode(Json, Options)
    catch
        error:Reason ->
            {error, {json_decode_error, Reason}}
    end.

%% Specialized encoding for streaming tokens
encode_streaming_token(Token) when is_binary(Token) ->
    % Ensure proper escaping for streaming
    Data = #{
        <<"type">> => <<"token">>,
        <<"content">> => Token,
        <<"timestamp">> => erlang:system_time(millisecond)
    },
    encode(Data, [
        {escape_forward_slashes, false},
        {use_nil, false},
        force_utf8
    ]);
encode_streaming_token(Token) when is_list(Token) ->
    encode_streaming_token(iolist_to_binary(Token));
encode_streaming_token(Token) ->
    encode_streaming_token(unicode:characters_to_binary(Token)).

%% Encode streaming response with metadata
encode_streaming_response(Response) ->
    Data = case Response of
        #{} -> Response;
        {Type, Content} ->
            #{
                <<"type">> => erlang:atom_to_binary(Type, utf8),
                <<"content">> => Content,
                <<"timestamp">> => erlang:system_time(millisecond)
            };
        Content when is_binary(Content); is_list(Content) ->
            #{
                <<"type">> => <<"content">>,
                <<"content">> => Content,
                <<"timestamp">> => erlang:system_time(millisecond)
            }
    end,
    encode(Data, [
        {escape_forward_slashes, false},
        {use_nil, false},
        force_utf8
    ]).

%% Decode streaming chunk from JSON
decode_streaming_chunk(Chunk) when is_binary(Chunk) ->
    case decode(Chunk) of
        {error, _} = Error -> Error;
        DecodedData -> {ok, DecodedData}
    end;
decode_streaming_chunk(Chunk) when is_list(Chunk) ->
    decode_streaming_chunk(iolist_to_binary(Chunk)).

%% Check if data is valid JSON
is_json(Data) when is_binary(Data) ->
    case decode(Data) of
        {error, _} -> false;
        _ -> true
    end;
is_json(_) -> false.

%% Internal helper functions

atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
atom_to_binary(Binary) when is_binary(Binary) ->
    Binary;
atom_to_binary(Other) ->
    unicode:characters_to_binary(io_lib:format("~w", [Other])).