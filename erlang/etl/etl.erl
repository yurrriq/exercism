-module(etl).

-export([transform/1]).

transform(X) -> orddict:to_list(orddict:from_list(lists:flatten(invert(X)))).

invert(Pairs) ->
    F = fun({Key, Values}, Acc1) ->
        G = fun(Value, Acc2) ->
            H = fun(Old) -> lists:flatten([Old] ++ [Key]) end,
            orddict:update(string:to_lower(Value), H, Key, Acc2)
        end,
        lists:foldl(G, Acc1, Values)
    end,
    lists:foldl(F, orddict:new(), Pairs).
