-module(sum_of_multiples).
-export([sumOfMultiplesDefault/1, sumOfMultiples/2]).

sumOfMultiplesDefault(N) -> sumOfMultiples([3,5], N).

sumOfMultiples(Mult, N) -> lists:sum(anyDivides(Mult, lists:seq(1, N - 1))).

anyDivides(Factors, Numbers) ->
  lists:filter(fun(N) -> lists:any(fun(D) -> divides(D, N) end, Factors) end,
               Numbers).

divides(D, N) -> N rem D =:= 0.
