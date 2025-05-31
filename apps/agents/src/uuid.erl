%% uuid.erl
%% Simple UUID (v4) generation for agent message IDs
-module(uuid).

-export([
    uuid4/0,
    generate/0,
    to_string/1
]).

%% Generate a version 4 UUID
-spec uuid4() -> uuid().
uuid4() ->
    % Generate 16 random bytes
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    
    % Set version 4 bits (random)
    C1 = (C band 16#0fff) bor 16#4000,
    
    % Set variant bits (RFC 4122)
    D1 = (D band 16#3fff) bor 16#8000,
    
    <<A:32, B:16, C1:16, D1:16, E:48>>.

%% Alias for uuid4
-spec generate() -> uuid().
generate() ->
    uuid4().

%% Convert a UUID to a string representation
-spec to_string(UUID) -> string() when
    UUID :: uuid().
to_string(<<A:32, B:16, C:16, D:16, E:48>>) ->
    lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b", 
                              [A, B, C, D, E])).

-type uuid() :: <<_:128>>.