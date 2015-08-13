-module(dna).
-export([hamming_distance/2]).

hamming_distance(Strand1, Strand2) ->
  zipfoldl(fun (X, Y, Z) -> Z + ne_to_int(X, Y) end, 0, Strand1, Strand2).

zipfoldl(F, Acc, [HA|TA], [HB|TB]) -> zipfoldl(F, F(HA, HB, Acc), TA, TB);
zipfoldl(_, Acc, [], []) -> Acc.

bool_to_int(true)  -> 1;
bool_to_int(false) -> 0.

ne_to_int(X, Y) -> bool_to_int(X =/= Y).
