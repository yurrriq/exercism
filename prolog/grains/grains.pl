/*

\PrologDialect{swiprolog}

\say{%
  There once was a wise servant who saved the life of a prince. The king
  promised to pay whatever the servant could dream up.  Knowing that the king
  loved chess, the servant told the king he would like to have grains of wheat.
  One grain on the first square of a chess board, with the number of grains
  doubling on each successive square.%
}

\Predicate square/2(SquareNumber, Value).

The number of grains of wheat on a particular square of the chessboard, where
\texttt{SquareNumber}$\ \in [1, 64]$.

\PL*/
square(SquareNumber, _) :-
    ( SquareNumber < 1
    ; SquareNumber > 64 ),
    !,
    fail.
/*PL

The number of grains of wheat on square $n$ is $2^{(n - 1)}$.

\PL*/
square(SquareNumber, Value) :- Value is 2 ** (SquareNumber - 1).
/*PL

\Predicate total/1(Value).

The total number of grains on the chessboard is one less than twice the number
of grains on the $64$th square.

\PL*/
total(Value) :-
    square(64, SixtyFourth),
    Value is 2 * SixtyFourth - 1.
/*PL
\EndProlog*/
