-module(raindrops).

-export([convert/1]).

-define(RAINDROPS, [
    {3, "Pling"},
    {5, "Plang"},
    {7, "Plong"}
]).

-spec convert(Number :: number()) -> Result :: string().
convert(Number) ->
    case lists:foldr(do_convert(Number), [], ?RAINDROPS) of
        [] -> integer_to_list(Number);
        List -> lists:flatten(List)
    end.

-spec do_convert(Number :: number()) ->
    fun(({Divisor :: number(), Drop :: string()}, Acc :: list(string())) -> string()).
do_convert(Number) ->
    fun
        ({Divisor, Drop}, Acc) when Number rem Divisor =:= 0 ->
            [Drop | Acc];
        (_, Acc) ->
            Acc
    end.
