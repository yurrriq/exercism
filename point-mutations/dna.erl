-module(dna).
-export([hamming_distance/2]).

bool_to_int(true)  -> 1;
bool_to_int(false) -> 0.

ne_to_int(X, Y) -> bool_to_int(X =/= Y).

hamming_distance(Strand1, Strand2) ->
  lists:sum(lists:zipwith(fun ne_to_int/2, Strand1, Strand2)).
