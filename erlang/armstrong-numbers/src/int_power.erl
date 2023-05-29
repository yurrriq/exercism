%% https://programming-idioms.org/idiom/32/integer-exponentiation-by-squaring
-module(int_power).
-export([exp/2]).

exp(X, N) -> exp(X, N, 1).

exp(_X, 0, Y) ->
    Y;
exp(X, N, Y) when N rem 2 =:= 0 ->
    exp(X * X, N div 2, Y);
exp(X, N, Y) ->
    exp(X * X, N div 2, X * Y).
