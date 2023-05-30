%%% ==================================================== [ perfect_numbers.erl ]
%%% @doc Perfect Numbers
%%% Determine if a number is `perfect', `abundant', or `deficient', based on
%%% Nicomachus's (60 - 120 CE) classification scheme for positive integers.
%%% @end
%%%
%%% @author Eric Bailey
%%% @copyright 2023 Eric Bailey
%%% @version 0.0.1
%%% @end
%%% ==================================================================== [ EOH ]
-module(perfect_numbers).

%%% Public API
-export([classify/1]).
-export_type([classification/0]).


%%% ============================================================= [ Public API ]

-type classification() ::
        deficient |
        perfect |
        abundant.
%% Nicomachus's classification scheme for positive integers.

%% @doc Classify an integer based on Nicomachus's scheme.
%% Throw an exception if `Number' is not a positive integer.
-spec classify(integer()) -> classification() | no_return().
classify(Number) when Number =< 0 ->
    erlang:error(invalid_number);
classify(Number) ->
    AliquotSum = aliquot_sum(Number),
    if
        AliquotSum =:= Number ->
            perfect;
        AliquotSum > Number ->
            abundant;
        AliquotSum < Number ->
            deficient
    end.

%%% ========================================================== [ Private Parts ]

%% @doc The sum of the factors of a number not including the number itself.
-spec aliquot_sum(pos_integer()) -> pos_integer().
aliquot_sum(Number) ->
    lists:sum([ Divisor || Divisor <- lists:seq(1, Number - 1),
                           Number rem Divisor =:= 0 ]).
%%% ==================================================================== [ EOF ]
