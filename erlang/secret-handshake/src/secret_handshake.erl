-module(secret_handshake).

-export([commands/1]).

-spec commands(non_neg_integer()) -> [string()].
commands(Number) when Number band 16 =:= 16 ->
    lists:reverse(commands(Number - 16));
commands(Number) when Number band 1 =:= 1 ->
    ["wink" | commands(Number - 1)];
commands(Number) when Number band 2 =:= 2 ->
    ["double blink" | commands(Number - 2)];
commands(Number) when Number band 4 =:= 4 ->
    ["close your eyes" | commands(Number - 4)];
commands(Number) when Number band 8 =:= 8 ->
    ["jump" | commands(Number - 8)];
commands(_) ->
    [].
