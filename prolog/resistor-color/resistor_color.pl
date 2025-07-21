:- module(resistor_color, [color_code/2, colors/1]).  /*

\PrologDialect{swiprolog}

\Predicate color_code/2(+Color:string, -Code:int).

True if \verb|Code| is the encoding of \verb|Color|.

\PL*/
:- semidet(color_code/2).

%! color_code(+Color:string, -Code:int) is semidet.
%
% True if =Code= is the encoding of =Color=.
color_code("black",  0).
color_code("brown",  1).
color_code("red",    2).
color_code("orange", 3).
color_code("yellow", 4).
color_code("green",  5).
color_code("blue",   6).
color_code("violet", 7).
color_code("grey",   8).
color_code("white",  9).
/*PL

\Predicate colors/1(-Colors).

The list of all known colors.

\PL*/
:- semidet(colors/1).

%! colors(-Colors:list(string)) is semidet.
%
% The list of all known colors.
%
% @see color_code/2
colors(Colors) :-
    findall(Color, color_code(Color, _), Colors).
/*PL

\EndProlog*/
