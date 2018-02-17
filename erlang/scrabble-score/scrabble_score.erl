-module(scrabble_score).

-export([score/1]).

-define(LEGEND,
        [{1,  "AEIOULNRST"},
         {2,  "DG"},
         {3,  "BCMP"},
         {4,  "FHVWY"},
         {5,  "K"},
         {8,  "JX"},
         {10, "QZ"}]).

-define(POINTS,
        maps:from_list(lists:flatten([{L,N} || {N,Ls} <- ?LEGEND, L <- Ls]))).

-spec score(string()) -> integer().
score(Word) -> lists:foldl(fun score/2, 0, Word).

-spec score(char(), integer()) -> integer().
score(C, Score) when C >= $a, C =< $z -> score(to_upper(C), Score);
score(C, Score) when C >= $A, C =< $Z -> #{C := N} = ?POINTS, Score + N.

-spec to_upper(char()) -> char().
to_upper(C) when C >= $a, C =< $z -> C - ($a - $A);
to_upper(C) when C >= $A, C =< $Z -> C.
