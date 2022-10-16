-module(secret_handshake).

-export([commands/1]).

-type operation() :: wink | 'double blink' | 'close your eyes' | jump.

-spec commands(non_neg_integer()) -> [string()].
commands(Number) ->
    lists:map(fun erlang:atom_to_list/1, handshake(Number, 1, [])).

-spec handshake(non_neg_integer(), 1 | 2 | 4 | 8 | 16, [operation()]) -> [operation()].
handshake(Number, 1, Operations) when Number band 1 =:= 1 ->
    handshake(Number, 2, [wink | Operations]);
handshake(Number, 2, Operations) when Number band 2 =:= 2 ->
    handshake(Number, 4, ['double blink' | Operations]);
handshake(Number, 4, Operations) when Number band 4 =:= 4 ->
    handshake(Number, 8, ['close your eyes' | Operations]);
handshake(Number, 8, Operations) when Number band 8 =:= 8 ->
    handshake(Number, 16, [jump | Operations]);
handshake(Number, 16, Operations) when Number band 16 =:= 16 ->
    Operations;
handshake(Number, Bit, Operations) when Bit < 16 ->
    handshake(Number, Bit * 2, Operations);
handshake(_, _, Operations) ->
    lists:reverse(Operations).
