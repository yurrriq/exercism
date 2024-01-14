:- module(leap). /*

\PrologDialect{swiprolog}

\Predicate divides/2(+Divisor:int, +Number:int).

\emph{Number} is a multiple of \emph{Divisor}.

Use the same
\href{https://www.swi-prolog.org/pldoc/man?section=operators}{precedence and
type} as \emph{rem} and similar operators.

\PL*/
:- op(400, yfx, divides).
divides(Divisor, Number) :-
/*PL

$0$ is the remainder of $Number \div Divisor$.

\PL*/
    0 is Number rem Divisor.
/*PL

\Predicate leap/1(+Year:int).

\emph{Year} is a \href{https://en.wikipedia.org/wiki/Leap_year}{leap year}.

If $400$ divides \emph{Year}, it is a leap year.
\href{https://en.wikipedia.org/wiki/Cut_(logic_programming)\#Green_cut}{Stop
looking for alternatives.}

\PL*/
leap(Year) :- 400 divides Year, !.
/*PL

If $100$ does not divide \emph{Year} and $4$ does, it is a leap year.

\PL*/
leap(Year) :-
    \+ 100 divides Year,
    4 divides Year.
/*PL

\EndProlog*/
