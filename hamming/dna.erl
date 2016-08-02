-module(dna).
-export([hamming_distance/2]).

hamming_distance(Strand1, Strand2) ->
    zipfoldl(fun(N1, N2, Sum) when N1 == N2 -> Sum;
                (_,  _,  Sum)               -> Sum + 1
             end, 0, Strand1, Strand2).

zipfoldl(F, Acc, [HA|TA], [HB|TB]) -> zipfoldl(F, F(HA, HB, Acc), TA, TB);
zipfoldl(_, Acc, [],      [])      -> Acc.
