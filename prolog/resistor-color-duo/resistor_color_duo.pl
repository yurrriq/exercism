:- module(resistor_color_duo, [value/2]).  /*

\PrologDialect{swiprolog}

\Predicate color_code/2(+Color:string, -Codeint).

True if \verb|Code| is the encoding of \verb|Color|.

\PL*/
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

\Predicate value/2(+Bands:list(string), -Value:int).

The encoded \verb|Value| of the first two \verb|Bands| of a resistor.

\PL*/
%! value(+Bands:list(string), -Value:int) is semidet.
%
% The encoded =Value= of the first two =Bands= of a resistor.
value([Color1, Color2], Value) :-
    color_code(Color1, Code1),
    color_code(Color2, Code2),
    Value is 10 * Code1 + Code2,
    !.
value([Color1, Color2|_], Value) :-
    value([Color1, Color2], Value).
/*PL

\EndProlog*/
