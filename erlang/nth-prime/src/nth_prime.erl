-module(nth_prime).

-export([prime/1]).

-spec prime(pos_integer()) -> pos_integer().
prime(1) ->
    2;
prime(2) ->
    3;
prime(N) when N >= 3 ->
    nth_prime(N, {2, [2, 3]}).

-spec nth_prime(_, {pos_integer(), [pos_integer(), ...]}) -> pos_integer().
nth_prime(N, {K, [Prime | _]}) when K =:= N ->
    Prime;
nth_prime(N, {K, Primes}) ->
    nth_prime(N, {K + 1, [next_prime(Primes) | Primes]}).

-spec next_prime([pos_integer(), ...]) -> pos_integer().
next_prime([Prime | Primes]) ->
    next_prime([Prime | Primes], Prime + 1).

-spec next_prime([pos_integer(), ...], pos_integer()) -> pos_integer().
next_prime(Primes, Candidate) ->
    case is_prime(Candidate, Primes) of
        true ->
            Candidate;
        _ ->
            next_prime(Primes, Candidate + 1)
    end.

-spec is_prime(pos_integer(), [pos_integer(), ...]) -> boolean().
is_prime(Candidate, Primes) ->
    not lists:any(fun(Prime) -> Candidate rem Prime =:= 0 end, Primes).
