-module(beer_song).

-export([verse/1, sing/1, sing/2]).

-spec verse(integer()) -> string().
verse(N) -> lists:map(fun (I) -> string:concat(line(I, N), ".\n") end, [0, 1]).

-spec line(0 | 1, integer()) -> string().
line(1, 0) -> buy_more(on_the_wall(99));
line(0, N) -> string:join([apply(fun (0, S) -> capitalize(S); (_, S) -> S end,
                                 [N, on_the_wall(N)]),
                           bottles_of_beer(N)], ", ");
line(1, N) -> string:concat(pass_around(take_down(N)), on_the_wall(N-1)).

-spec sing(integer()) -> string().
sing(From) -> sing(From, 0).

-spec sing(integer(), integer()) -> string().
sing(From, To) ->
  lists:map(fun (N) -> verse(N) ++ ["\n"] end, lists:seq(From, To, -1)).

-spec bottles(integer()) -> string().
bottles(0) -> "no more bottles";
bottles(1) -> "1 bottle";
bottles(N) -> string:concat(integer_to_list(N), " bottles").

-spec bottles_of_beer(integer()) -> string().
bottles_of_beer(N) -> string:concat(bottles(N), " of beer").

-spec on_the_wall(integer()) -> string().
on_the_wall(N) -> string:concat(bottles_of_beer(N), " on the wall").

-spec buy_more(string()) -> string().
buy_more(S) -> string:concat("Go to the store and buy some more, ", S).

-spec take_down(integer() | string()) -> string().
take_down(1)                    -> take_down("it");
take_down(N) when is_integer(N) -> take_down("one");
take_down(S) when is_list(S)    -> string:join(["Take", S, "down"], " ").

-spec pass_around(string()) -> string().
pass_around(S) -> string:concat(S, " and pass it around, ").

-spec capitalize(string()) -> string().
capitalize([FirstLetter | String]) -> string:to_upper([FirstLetter]) ++ String.
