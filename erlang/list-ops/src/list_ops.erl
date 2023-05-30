-module(list_ops).

-export([
    append/2,
    concat/1,
    filter/2,
    length/1,
    map/2,
    foldl/3,
    foldr/3,
    reverse/1
]).

-spec append(List1, List2) -> List3 when
    List1 :: [T],
    List2 :: [T],
    List3 :: [T],
    T :: term().
append(Xs, Ys) when is_list(Xs), is_list(Ys) ->
    do_foldr(fun cons/2, Ys, Xs).

-spec concat(Lists) -> List when
    Lists :: [[T]],
    List :: [T],
    T :: term().
concat([]) ->
    [];
concat(Lists = [List | _]) when is_list(List) ->
    do_foldr(fun append/2, [], Lists).

-spec filter(Pred, [A]) -> [A] when
    Pred :: fun((A) -> boolean()),
    A :: term().
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

-spec length([_]) -> non_neg_integer().
length([_ | Tail]) ->
    1 + ?MODULE:length(Tail);
length([]) ->
    0.

-spec map(Fun, [A]) -> [B] when
    Fun :: fun((A) -> B),
    A :: term(),
    B :: term().
map(F, List) when is_function(F, 1) ->
    do_map(F, List).

do_map(F, [X | Xs]) ->
    [F(X) | do_map(F, Xs)];
do_map(_F, []) ->
    [].

-spec foldl(Fun, B, [A]) -> B when
    Fun :: fun((A, B) -> B),
    A :: term(),
    B :: term().
foldl(F, Acc, List) when is_function(F, 2) ->
    do_foldl(F, Acc, List).

do_foldl(F, Acc, [X | Xs]) ->
    do_foldl(F, F(X, Acc), Xs);
do_foldl(_F, Acc, []) ->
    Acc.

-spec foldr(Fun, B, [A]) -> B when
    Fun :: fun((A, B) -> B),
    A :: term(),
    B :: term().
foldr(F, Acc, List) when is_function(F, 2) ->
    do_foldr(F, Acc, List).

do_foldr(F, Acc, [X | Xs]) ->
    F(X, do_foldr(F, Acc, Xs));
do_foldr(_F, Acc, []) ->
    Acc.

-spec reverse([Elem]) -> [Elem] when
    Elem :: term().
reverse([X | Xs]) ->
    do_reverse(Xs, [X]);
reverse([]) ->
    [].

do_reverse([X | Xs], Acc) ->
    do_reverse(Xs, [X | Acc]);
do_reverse([], Acc) ->
    Acc.

-spec cons(Head, [Head]) -> [Head, ...] when
    Head :: term().
cons(X, [Y | Ys]) ->
    [X, Y | Ys];
cons(X, []) ->
    [X].
