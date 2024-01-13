/*

\PrologDialect{swiprolog}

Use the \href{https://www.swi-prolog.org/man/clpfd.html}{Constraint Logic Programming over Finite Domains} library.

\PL*/
:- use_module(library(clpfd)).
/*PL

\Predicate create/1(+Tuple).

\emph{(Row, Col)} represents a valid chessboard position.

\PL*/
create((Row, Col)) :-
/*PL

\emph{Row} and \emph{Col} are both elements of $0..7$.

\PL*/
    [Row, Col] ins 0..7.
/*PL

\Predicate attack/2(+Tuple, +Tuple).

A queen positioned at \emph{(Row1, Col1)} is vulnerable to an attack by another
queen positioned at \emph{(Row2, Col2)}.

\PL*/
attack((Row1, Col1), (Row2, Col2)) :-
/*PL

Ensure both positions are valid.

\PL*/
    create((Row1, Col1)),
    create((Row2, Col2)),
/*PL

Queens in the same row can attack each other.

\PL*/
    ( Row1 #= Row2
/*PL

Queens in the same column can attack each other.

\PL*/
    ; Col1 #= Col2
/*PL

Queens that share the same diagonal can attack each other.

\PL*/
    ; abs(Row1 - Row2) #= abs(Col1 - Col2)
    ).
/*PL

\EndProlog*/
