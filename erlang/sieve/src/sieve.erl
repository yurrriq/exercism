-module(sieve).

-export([primes/1]).

%% @doc Compute the list of primes from 2 up to a given number.
-spec primes(Limit :: pos_integer()) -> [Primes :: pos_integer()].
primes(Limit) when Limit < 2 ->
    [];
primes(Limit) ->
    primes(2, round(math:sqrt(Limit)), [], lists:seq(3, Limit, 2)).

-spec primes(Prime, Limit, Primes, Candidates) -> Primes when
    Prime :: pos_integer(),
    Limit :: pos_integer(),
    Primes :: [pos_integer()],
    Candidates :: [pos_integer()].
primes(Prime, Limit, Primes, Candidates) when Prime > Limit ->
    lists:reverse([Prime | Primes]) ++ Candidates;
primes(Prime, Limit, Primes, Candidates) ->
    [NextPrime | NextCandidates] = [N || N <- Candidates, N rem Prime =/= 0],
    primes(NextPrime, Limit, [Prime | Primes], NextCandidates).
