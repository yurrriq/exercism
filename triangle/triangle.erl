-module(triangle).

-export([kind/3]).

-type kind() :: equilateral | isosceles | scalene.

-spec kind(integer(), integer(), integer()) -> kind().
kind(A,B,C) when A =< 0; B =< 0; C =< 0 ->
  {error, "all side lengths must be positive"};
kind(A,B,C) when A >= B + C; B >= C + A; C >= A + B ->
  {error, "side lengths violate triangle inequality"};
kind(A,A,A) -> equilateral;
kind(A,A,_) -> isosceles;
kind(A,_,A) -> isosceles;
kind(_,B,B) -> isosceles;
kind(_,_,_) -> scalene.
