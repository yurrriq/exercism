:- module(collatz_conjecture, [collatz_steps/2]).

%!  collatz_steps(+N:int, -Steps:int) is det.
%
%   Compute the number of =Steps= required to reach 1.
collatz_steps(1, 0) :- !.
collatz_steps(N, Steps) :-
    must_be(integer, N),
    N > 1,
    (  even(N)
    -> Next is N // 2
    ;  Next is 3 * N + 1
    ),
    collatz_steps(Next, NextSteps),
    Steps is 1 + NextSteps.

%!  even(+N:int) is det.
%   True if =N= is even.
even(N) :-
    0 is N mod 2.
