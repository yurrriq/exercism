-module(list_ops).

-export([append/2, concat/1, filter/2, length/1, map/2, foldl/3, foldr/3,
         reverse/1]).

append(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    foldr(fun cons/2, Ys, Xs).

concat([]) ->
    [];
concat(Lists=[List|_]) when is_list(List) ->
    foldr(fun append/2, [], Lists).

filter(Pred, List) when is_function(Pred, 1) ->
    F = fun(Y, Acc) ->
                case Pred(Y) of
                    true ->
                        [Y | Acc];
                    false ->
                        Acc
                end
        end,
    do_foldr(F, [], List).

length([_|Tail]) ->
    1 + ?MODULE:length(Tail);
length([]) ->
    0.

map(F, List) when is_function(F, 1) ->
    do_map(F, List).

do_map(F, [X | Xs]) ->
    [F(X) | do_map(F, Xs)];
do_map(_F, []) ->
    [].

foldl(F, Acc, List) when is_function(F, 2) ->
    do_foldl(F, Acc, List).

do_foldl(F, Acc, [X | Xs]) ->
    do_foldl(F, F(X, Acc), Xs);
do_foldl(_F, Acc, []) ->
    Acc.

foldr(F, Acc, List) when is_function(F, 2) ->
    do_foldr(F, Acc, List).

do_foldr(F, Acc, [X | Xs]) ->
    F(X, do_foldr(F, Acc, Xs));
do_foldr(_F, Acc, []) ->
    Acc.

reverse(List) ->
    foldl(fun cons/2, [], List).

cons(Head, Tail) ->
    [Head | Tail].
