-module(series).
-export([from_string/2]).

from_string(Len, Str) ->
  [lists:sublist(Str, N, Len) || N <- lists:seq(1, length(Str) - Len + 1)].
