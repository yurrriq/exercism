/*

\PrologDialect{swiprolog}

\Predicate isogram/1(Phrase).

Determine if a word or phrase is an isogram.

\PL*/
isogram(Phrase) :-
/*PL

Convert \emph{Phrase} to lower case and unify the result with
\emph{LowerPhrase}.

\PL*/
    string_lower(Phrase, LowerPhrase),
/*PL

Convert \emph{LowerPhrase} to a list of character codes and unify the result
with \emph{Codes}.

\PL*/
    string_codes(LowerPhrase, Codes),
/*PL

Filter elements of \emph{Codes} for which \texttt{is\_alpha/1} succeeds and
unify the result with \emph{Letters}.

\PL*/
    include(is_alpha, Codes, Letters),
/*PL

True if \emph{Letters} is a proper list without duplicates.

\PL*/
    is_set(Letters).
/*PL

\EndProlog*/
