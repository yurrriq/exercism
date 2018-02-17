-module(strain).
-export([
  discard/2,
  keep/2
]).

discard(Pred, List) ->
  [X || X <- List, not(Pred(X))].

keep(Pred, List) ->
  [X || X <- List, Pred(X)].
