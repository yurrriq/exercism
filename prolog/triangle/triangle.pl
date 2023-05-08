/*

\PrologDialect{swiprolog}

For a shape to be a triangle at all, all sides have to be of length $\> 0$, and
the sum of the lengths of any two sides must be greater than or equal to the
length of the third side.

\Predicate has_positive_sides/3(Side1, Side2, Side3).

All three side lengths must be positive.

\PL*/
has_positive_sides(Side1, Side2, Side3) :-
    Side1 > 0,
    Side2 > 0,
    Side3 > 0.
/*PL

\Predicate satisfies_triangle_inequality/3(Side1, Side2, Side3).

The sum of the lengths of any two sides must be greater than or equal to the
length of the third side.

\PL*/
satisfies_triangle_inequality(Side1, Side2, Side3) :-
    Side1 < Side2 + Side3,
    Side2 < Side3 + Side1,
    Side3 < Side1 + Side2.

/*PL

\Predicate triangle/4(Side1, Side2, Side3, Classification).

Determine if a triangle is equilateral, isosceles, or scalene.

\PL*/
triangle(Side1, Side2, Side3, _) :-
    ( \+ has_positive_sides(Side1, Side2, Side3)
    ; \+ satisfies_triangle_inequality(Side1, Side2, Side3)),
    !,
    fail.
/*PL

An \textit{equilateral} triangle has all three sides the same length.

\PL*/
triangle(Side, Side, Side, "equilateral") :- !.
/*PL

An \textit{isosceles} triangle has at least two sides the same length. (It is
sometimes specified as having exactly two sides the same length, but for the
purposes of this exercise we'll say at least two.)

\PL*/
triangle(Side, Side, _, "isosceles") :- !.
triangle(Side, _, Side, "isosceles") :- !.
triangle(_, Side, Side, "isosceles") :- !.
/*PL

A \textit{scalene} triangle has all sides of different lengths.

\PL*/
triangle(Side1, Side2, Side3, "scalene") :-
    dif(Side1, Side2),
    dif(Side1, Side3),
    dif(Side2, Side3),
    !.
/*PL
\EndProlog*/
