-module(yacht).

-export([score/2]).
-export_type([category/0]).

-type category() ::
    ones
    | twos
    | threes
    | fours
    | fives
    | sixes
    | full_house
    | four_of_a_kind
    | little_straight
    | big_straight
    | choice
    | yacht.

-spec score([1..6], category()) -> non_neg_integer() | {error, invalid_roll}.
score(Dice, ones) ->
    pips(Dice, 1);
score(Dice, twos) ->
    pips(Dice, 2);
score(Dice, threes) ->
    pips(Dice, 3);
score(Dice, fours) ->
    pips(Dice, 4);
score(Dice, fives) ->
    pips(Dice, 5);
score(Dice, sixes) ->
    pips(Dice, 6);
score(Dice, full_house) ->
    case lists:sort(Dice) of
        [X, X, Y, Y, Y] when X =/= Y ->
            lists:sum(Dice);
        [X, X, X, Y, Y] when X =/= Y ->
            lists:sum(Dice);
        _ ->
            0
    end;
score(Dice, four_of_a_kind) ->
    case lists:sort(Dice) of
        [X, X, X, X, _] ->
            4 * X;
        [_, Y, Y, Y, Y] ->
            4 * Y;
        _ ->
            0
    end;
score(Dice, little_straight) ->
    case lists:sort(Dice) of
        [1, 2, 3, 4, 5] ->
            30;
        _ ->
            0
    end;
score(Dice, big_straight) ->
    case lists:sort(Dice) of
        [2, 3, 4, 5, 6] ->
            30;
        _ ->
            0
    end;
score(Dice, choice) ->
    lists:sum(Dice);
score([X, X, X, X, X], yacht) ->
    50;
score([_, _, _, _, _], _Category) ->
    0;
score(_Dice, _Category) ->
    {error, invalid_roll}.

-spec pips([1..6], 1..6) -> non_neg_integer().
pips(Dice, N) ->
    lists:foldl(do_pips(N), 0, Dice).

do_pips(N) ->
    fun
        (Die, Sum) when Die == N ->
            Sum + N;
        (_, Sum) ->
            Sum
    end.
