%%% @author Eric Bailey
%%% @doc Integer square roots.
-module(square_root).

-export([square_root/1]).

%% @doc Compute the positive integer square root of a given positive integer.
-spec square_root(Radicand :: pos_integer()) -> SquareRoot :: pos_integer().
square_root(Radicand) ->
    do_square_root(Radicand, 0, Radicand).

-spec do_square_root(
    Radicand :: pos_integer(), Low :: non_neg_integer(), High :: pos_integer()
) ->
    SquareRoot :: pos_integer().
do_square_root(_Radicand, Low, High) when Low > High ->
    High;
do_square_root(Radicand, Low, High) ->
    Mid = (Low + High) div 2,
    if
        Mid * Mid =< Radicand ->
            do_square_root(Radicand, Mid + 1, High);
        true ->
            do_square_root(Radicand, Low, Mid - 1)
    end.
