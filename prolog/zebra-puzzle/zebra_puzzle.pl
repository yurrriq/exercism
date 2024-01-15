:- module(zebra_puzzle). /*

\PrologDialect{swiprolog}

\Predicate adjacent_to/2(?X, ?Y, ?List).

The element \emph{X} is next to \emph{Y} in \emph{List}, i.e. \emph{Y} directly
follows \emph{X} or \emph{X} directly follows \emph{Y}.  Not to be confused with
\href{https://www.swi-prolog.org/pldoc/doc_for?object=nextto/3}{nextto/3}.

\PL*/
adjacent_to(X, Y, List) :-
    ( nextto(X, Y, List)
    ; nextto(Y, X, List)
    ).
/*PL

\Predicate puzzle/1(?Houses).

The puzzle consists of 15 clues about \emph{Houses}.

\begin{enumerate}
\PL*/
puzzle(Houses) :-
/*PL

\item There are five houses.

\PL*/
    length(Houses, 5),
/*PL

\item The Englishman lives in the red house.

\PL*/
    member(house(englishman, red, _, _, _), Houses),
/*PL

\item The Spaniard owns the dog.

\PL*/
    member(house(spainard, _, dog, _, _), Houses),
/*PL

\item Coffee is drunk in the green house.

\PL*/
    member(house(_, green, _, coffee, _), Houses),
/*PL

\item The Ukrainian drinks tea.

\PL*/
    member(house(ukrainian, _, _, tea, _), Houses),
/*PL

\item The green house is immediately to the right of the ivory house.

\PL*/
    nextto(house(_, ivory, _, _, _), house(_, green, _, _, _), Houses),
/*PL

\item The Old Gold smoker owns snails.

\PL*/
    member(house(_, _, snails, _, old_gold), Houses),
/*PL

\item Kools are smoked in the yellow house.

\PL*/
    member(house(_, yellow, _, _, kool), Houses),
/*PL

\item Milk is drunk in the middle house.

\PL*/
    [_, _, house(_, _, _, milk, _), _, _] = Houses,
/*PL

\item The Norwegian lives in the first house.

\PL*/
    [house(norwegian, _, _, _, _) | _ ] = Houses,
/*PL

\item The man who smokes Chesterfields lives in the house next to the man with
the fox.

\PL*/
    adjacent_to(house(_, _, _, _, chesterfield), house(_, _, fox, _, _), Houses),
/*PL

\item Kools are smoked in the house next to the house where the horse is kept.

\PL*/
    adjacent_to(house(_, _, _, _, kool), house(_, _, horse, _, _), Houses),
/*PL

\item The Lucky Strike smoker drinks orange juice.

\PL*/
    member(house(_, _, _, orange_juice, lucky_strike), Houses),
/*PL

\item The Japanese smokes Parliaments.

\PL*/
    member(house(japanese, _, _, _, parliament), Houses),
/*PL

\item The Norwegian lives next to the blue house.

\PL*/
    adjacent_to(house(norwegian, _, _, _, _), house(_, blue, _, _, _), Houses).
/*PL

\end{enumerate}

\Predicate zebra_owner/1(-Owner).

Determine the nationality of the zebra \emph{Owner}.

\PL*/
zebra_owner(Owner) :-
    puzzle(Houses),
    member(house(Owner, _, zebra, _, _), Houses).
/*PL

\Predicate water_drinker/1(-Drinker).

Determine the nationality of the water \emph{Drinker}.

\PL*/
water_drinker(Drinker) :-
    puzzle(Houses),
    member(house(Drinker, _, _, water, _), Houses).
/*PL

\EndProlog*/
