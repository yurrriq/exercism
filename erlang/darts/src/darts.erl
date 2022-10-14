-module(darts).

-export([score/2]).

score(X, Y) ->
    Distance = distance_from_origin(X, Y),
    do_score(Distance).

do_score(Distance) when Distance > 10 -> 0;
do_score(Distance) when Distance > 5 -> 1;
do_score(Distance) when Distance > 1 -> 5;
do_score(_Distance) -> 10.

distance_from_origin(X, Y) -> math:sqrt(X * X + Y * Y).
