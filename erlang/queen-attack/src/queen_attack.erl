-module(queen_attack).

-export([can_attack/2]).

-type position() :: {1..8, 1..8}.

-spec can_attack(position(), position) -> boolean().
can_attack({Row, _}, {Row, _}) -> true;
can_attack({_, Col}, {_, Col}) -> true;
can_attack({Row1, Col1}, {Row2, Col2}) -> abs(Row1 - Row2) =:= abs(Col1 - Col2).
