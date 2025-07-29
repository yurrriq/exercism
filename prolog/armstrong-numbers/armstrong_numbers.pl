:- module(armstrong_numbers, [armstrong_number/1]).

%!  armstrong_number(+Number:int) is det.
%
%   True if =Number= is an Armstrong number in base 10.
armstrong_number(0) :- !.
armstrong_number(Number) :-
    must_be(positive_integer, Number),
    digits(10, Number, Digits),
    length(Digits, Length),
    maplist({Length}/[Digit, Power]>>(Power is Digit^Length), Digits, Powers),
    sum_list(Powers, Number).

%!  digits(+Base:int, +Number:int, -Digits:list(int)) is det.
%
%   Convert nonnegative integer =N= to its list of =Digits= in base =Base= in
%   reverse order.
digits(_, 0, [0]) :- !.
digits(Base, Number, Digits) :-
    must_be(integer, Base),
    Base >= 2,
    must_be(positive_integer, Number),
    (  Number =:= 0
    -> Digits = [0]
    ;  must_be(positive_integer, Number),
       divmod(Number, Base, Quotient, Digit),
       (  Quotient =:= 0
       -> Digits = [Digit]
       ;  digits(Base, Quotient, Rest),
          Digits = [Digit|Rest]
       )
    ).
