-module(trinary).
-export([to_decimal/1]).

-define(BASE, 3).

-spec to_decimal(string()) -> integer().
to_decimal(String) -> to_decimal(String, 0).

-spec to_decimal(string(), integer()) -> integer().
to_decimal([], Acc) -> Acc;
to_decimal([C|Cs], Acc) when C >= $0, C =< $2 ->
  to_decimal(Cs, Acc * ?BASE + (C - $0));
to_decimal(_S, _Acc) -> 0.
