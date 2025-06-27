/*

\PrologDialect{swiprolog}

\Predicate chatty/1(?Person).
\label{pred:chatty/1}

The \emph{Person} is chatty.

\PL*/
chatty(gustavo).
chatty(valeria).
/*PL

\Predicate likes/2(?First, ?Second).
\label{pred:likes/2}

The \emph{First} person likes the \emph{Second} person.

\PL*/
likes(esteban, malena).
likes(malena, esteban).
likes(gustavo, valeria).
/*PL

\Predicate pairing/2(?First, ?Second).
\label{pred:pairing/2}

Two people make a good pairing if they both \hyperref[pred:likes/2]{like} each
other.

\PL*/
pairing(First, Second) :-
    likes(First, Second),
    likes(Second, First).
/*PL

\hyperref[pred:chatty/1]{Chatty} people intermingle easily, so at least one
chatty person makes for a good pairing.

\PL*/
pairing(First, Second) :-
    chatty(First);
    chatty(Second).
/*PL

\Predicate seating/5(?A, ?B, ?C, ?D, ?E).

A seating arrangement at a round table for five people consists of five good
\hyperref[pred:pairing/2]{pairings}.

\PL*/
seating(A, B, C, D, E) :-
    pairing(A, B),
    pairing(B, C),
    pairing(C, D),
    pairing(D, E),
    pairing(E, A).
/*PL

\EndProlog*/
