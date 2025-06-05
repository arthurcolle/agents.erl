%% jiffy.erl
%% Simple JSON library replacement for missing jiffy dependency
-module(jiffy).
-export([encode/1, encode/2, decode/1, decode/2]).

%% Encode Erlang terms to JSON
encode(Term) ->
    encode(Term, []).

encode(Term, _Options) ->
    jsx:encode(Term).

%% Decode JSON to Erlang terms
decode(JSON) ->
    decode(JSON, []).

decode(JSON, Options) ->
    jsx:decode(JSON, Options).