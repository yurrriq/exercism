:- module(difference_of_squares, [square_of_sum/2, sum_of_squares/2, difference/2]).  /*

\PrologDialect{swiprolog}

\Predicate square_of_sum/2(+N:int, -Result:int).

The square of the sum of the first $n$ numbers, i.e.,
\(
    \displaystyle
    \left(\sum_{k=1}^{n} k\right)^{2} = \left(\frac{n(n + 1)}{2}\right)^{2}
\).

\PL*/
%!  square_of_sum(+N:int, -Result:int) is semidet.
%
%   The square of the sum of the first =N= natural numbers.
square_of_sum(N, Result) :-
    Result is ((N * (N + 1)) / 2)^2.
/*PL

\Predicate sum_of_squares/2(+N:int, -Result:int).

The sum of the squares of the first $n$ natural numbers, i.e.,
\(
    \displaystyle
    \sum_{k=1}^{n} k^2 = \frac{n(n + 1)(2n + 1)}{6}
\).

\PL*/
%!  sum_of_squares(+N:int, -Result:int) is semidet.
%
%   The sum of the squares of the first =N= natural numbers.
sum_of_squares(N, Result) :-
    Result is (N * (N + 1) * (2 * N + 1)) / 6.
/*PL

\Predicate difference/2(+N:int, -Result).

The difference between the square of the sum of the first $n$ natural numbers
and the sum of the squares of the first $n$ natural numbers, i.e.,
\(
    \displaystyle
    \left(\sum_{k=1}^{n} k\right)^{2} - \sum_{k=1}^{n} k^{2}.
\)

\PL*/
%!  difference(+N:int, -Result:int) is semidet.
%
%   The difference between the square of the sum of the first =N= natural
%   numbers and the sum of the squares of the first =N= natural numbers.
%
%   @see square_of_sum/2, sum_of_squares/2.
difference(N, Result) :-
    square_of_sum(N, SquareOfSum),
    sum_of_squares(N, SumOfSquares),
    Result is SquareOfSum - SumOfSquares.
/*PL

\EndProlog*/
