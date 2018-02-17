-module(grains).
-export([square/1, total/0]).

square(N) -> trunc(math:pow(2, N-1)).

total() -> square(65) - 1.
%% total() -> trunc(math:pow(2, 64)) - 1.
%% total() -> 18446744073709551615.
