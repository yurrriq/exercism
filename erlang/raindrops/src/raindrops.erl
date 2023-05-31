-module(raindrops).

-export([convert/1]).

-define(RAINDROPS, [
    {3, "Pling"},
    {5, "Plang"},
    {7, "Plong"}
]).

-spec convert(Number :: non_neg_integer()) -> string().
convert(Number) ->
    do_convert(Number, [], ?RAINDROPS).

-spec do_convert(
    Number :: non_neg_integer(),
    Sounds :: [string()],
    Raindrops :: [{non_neg_integer(), string()}]
) ->
    string().
do_convert(Number, [], []) ->
    integer_to_list(Number);
do_convert(_, Sounds, []) ->
    lists:concat(lists:reverse(Sounds));
do_convert(Number, Sounds, [{Divisor, Sound} | Raindrops]) when
    Number rem Divisor =:= 0
->
    do_convert(Number, [Sound | Sounds], Raindrops);
do_convert(Number, Sounds, [_ | Raindrops]) ->
    do_convert(Number, Sounds, Raindrops).
