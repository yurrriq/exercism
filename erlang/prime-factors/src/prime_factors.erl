%%% ====================================================== [ prime_factors.erl ]
%%% @doc Prime Factors
%%% Compute the prime factors of a given natural number.
%%% @end
%%%
%%% @author Eric Bailey
%%% @copyright 2023 Eric Bailey
%%% @version 0.0.1
%%% @end
%%% ==================================================================== [ EOH ]
-module(prime_factors).

%%% Public API.
-export([factors/1]).

%%% ============================================================= [ Public API ]

%% @doc Compute the prime factors of a given natural `Number'.
-spec factors(Number :: pos_integer()) -> Factors :: [pos_integer(), ...].
factors(Number) ->
    do_factors(Number, 2, []).

%%% ========================================================== [ Private Parts ]

%% @doc Internal implementation of {@link factors/1}.
-spec do_factors(
    Number :: pos_integer(),
    Divisor :: pos_integer(),
    FactorsAcc :: [pos_integer()]
) -> Factors :: [pos_integer()].
%% If `Number' is 1, there are no (additional) prime factors.
do_factors(1, _, Factors) ->
    Factors;
%% If `Divisor' divides `Number', prepend it to `Factors', divide `Number' by
%% `Divisor' and repeat.
do_factors(Number, Divisor, Factors) when Number rem Divisor =:= 0 ->
    do_factors(Number div Divisor, Divisor, [Divisor | Factors]);
%% If 2 does not divide `Number', the next trial divisor is 3.
do_factors(Number, 2, Factors) ->
    do_factors(Number, 3, Factors);
%% Otherwise, add 2 to `Divisor' and repeat.
do_factors(Number, Divisor, Factors) ->
    do_factors(Number, Divisor + 2, Factors).

%%% ==================================================================== [ EOF ]
