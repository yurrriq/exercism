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
score(Word) -> lists:foldl(fun add_points/2, 0, Word).

add_points(C, Score) when C >= $a, C =< $z ->
  add_points(to_upper(C), Score);
add_points(C, Score) when C >= $A, C =< $Z ->
  #{C := Points} = ?POINTS,
  Score + Points.

to_upper(C) when C >= $a, C =< $z -> C - 32; %% 32 =:= $a - $A
to_upper(C) when C >= $A, C =< $Z -> C.
