/*

Albert and Benard have just become friends with Cheryl, and they want to know
when her birthday is.

\Predicate possible_date/2(Month, Day).

Cheryl gives them a list of 10 possible dates.
\PL*/
possible_date(may, 15).
possible_date(may, 16).
possible_date(may, 19).
possible_date(june, 17).
possible_date(june, 18).
possible_date(july, 14).
possible_date(july, 16).
possible_date(august, 14).
possible_date(august, 15).
possible_date(august, 17).
/*PL

Cheryl then tells Albert and Bernard separately the month and the day of her
birthday, respectively.

Albert says, \say{I don't know when Cheryl's birthday is, but I know that
Bernard doesn't know either.}

If Albert knows the month but doesn't know the date, then the month does not
uniquely determine the day.

\Predicate month_uniquely_determines_day/1(Month).

If a month uniquely determines the day, it means that the month has only one
possible day.

\PL*/
month_uniquely_determines_day(Day) :-
    possible_date(Month, Day),
    findall(D, possible_date(Month, D), [_]).
/*PL

If Albert knows that knowning the day alone isn't enough information, then the
day does not uniquely determine the month.

\Predicate day_uniquely_determines_month/1(Month).

If a day uniquely determines the month, it means that the day occurs in only one
possible month.

\PL*/
day_uniquely_determines_month(Month) :-
    possible_date(Month, Day),
    findall(M, possible_date(M, Day), [_]).
/*PL

\Predicate albert_knows_month/2(Month, Day).

So, Albert knows that the month does not uniquely determine the day, nor does
the day uniquely determine the month.

\PL*/
albert_knows_month(Month, Day) :-
    possible_date(Month, Day),
    \+(month_uniquely_determines_day(Day)),
    \+(day_uniquely_determines_month(Month)).
/*PL

Bernard says, \say{At first I didn't know when Cheryl's birthday is, but I know
now.}

\Predicate bernard_knows/2(Month, Day).

Since Bernard knows the day, and now knows that the month does not uniquely
determine the day, he can deduce the month.

\PL*/
bernard_knows(Month, Day) :-
    possible_date(Month, Day),
    findall(M, albert_knows_month(M, Day), [Month]).
/*PL

Albert says, \say{Then I also know when Cheryl's birthday is.}

\Predicate albert_knows/2(Month, Day).

Since Albert knows the month, and now knows that knowing the day and that the
month does not uniquely determine the day determines the month, Albert can
deduce the day.

\PL*/
albert_knows(Month, Day) :-
    possible_date(Month, Day),
    findall(D, bernard_knows(Month, D), [Day]).

cheryls_birthday(Month, Day) :-
    bernard_knows(Month, Day),
    albert_knows(Month, Day).
/*PL
\EndProlog*/
