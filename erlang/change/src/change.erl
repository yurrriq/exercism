-module(change).

-export([find_fewest_coins/2]).

-type option(A) :: {some, A} | none.

-spec find_fewest_coins(Target :: non_neg_integer(), Coins :: [pos_integer()]) ->
    [pos_integer()] | undefined.
find_fewest_coins(0, _Coins) ->
    [];
find_fewest_coins(Target, _Coins) when Target < 0 ->
    erlang:error(negative_target);
find_fewest_coins(Target, Coins) ->
    [LowestDenomination | _] = SortedCoins = lists:sort(Coins),
    case
        do_find_fewest_coins(
            1, Target div LowestDenomination, Target, SortedCoins
        )
    of
        {some, Solution} ->
            Solution;
        none ->
            undefined
    end.

-spec do_find_fewest_coins(
    N :: pos_integer(),
    Limit :: pos_integer(),
    Target :: pos_integer(),
    Coins :: [pos_integer()]
) -> option([pos_integer()]).
do_find_fewest_coins(N, Limit, Target, Coins) ->
    case knapsack(N, Target, Coins) of
        none when N =< Limit ->
            do_find_fewest_coins(N + 1, Limit, Target, Coins);
        MaybeSolution ->
            MaybeSolution
    end.

-spec knapsack(
    N :: non_neg_integer(),
    Goal :: pos_integer(),
    Items :: [pos_integer()]
) ->
    option([pos_integer()]).
knapsack(0, _Goal, _Items) ->
    none;
knapsack(1, Goal, Items) ->
    case lists:member(Goal, Items) of
        true -> {some, [Goal]};
        false -> none
    end;
knapsack(N, Goal, Items) when N > 0 ->
    do_knapsack(N, Goal, Items).

-spec do_knapsack(
    N :: pos_integer(),
    Goal :: pos_integer(),
    Items :: [pos_integer()]
) ->
    option([pos_integer()]).
do_knapsack(_N, _Goal, []) ->
    none;
do_knapsack(_N, Goal, [Item | _Items]) when Item > Goal ->
    none;
do_knapsack(N, Goal, [Item | Items]) ->
    case knapsack(N - 1, Goal - Item, [Item | Items]) of
        none ->
            do_knapsack(
                N, Goal, lists:dropwhile(fun(X) -> X == Item end, Items)
            );
        {some, Solution} ->
            {some, [Item | Solution]}
    end.
