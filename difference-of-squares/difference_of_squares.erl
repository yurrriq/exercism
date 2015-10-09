-module(difference_of_squares).

-export([square_of_sums/1, sum_of_squares/1]).

%% \left(\sum\limits_{k=1}^n k\right)^2 = \left(\frac{n(n + 1)}{2}\right)^2
square_of_sums(N) -> trunc(math:pow((N * (N + 1)) / 2, 2)).

%% \sum\limits_{k=1}^n k^2 = \frac{n(n + 1)(2n + 1)}{6}
sum_of_squares(N) -> trunc((N * (N + 1) * ((2 * N) + 1)) / 6).
