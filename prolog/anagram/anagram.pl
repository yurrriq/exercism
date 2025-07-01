:- module(anagram, [is_anagram/2, anagram/3]).  /*

\PrologDialect{swiprolog}

\Predicate msort_string_lower/3(+String:string, -Lower:string, -Sorted:string).

Sort the list of characters of \verb|String| converted to lower case, and unify
the results with \verb|Sorted| and \verb|Lower|, respectively.

\PL*/
msort_string_lower(String, Lower, Sorted) :-
    string_lower(String, Lower),
    atom_chars(Lower, Chars),
    msort(Chars, Sorted).
/*PL

\Predicate is_anagram/2(?Word, ?Candidate).

True if \verb|Word| is an anagram of \verb|Candidate|.

\PL*/
is_anagram(Word, Candidate) :-
    msort_string_lower(Word, WordLower, Sorted),
    is_anagram(WordLower, Sorted, Candidate).
/*PL

\Predicate is_anagram/3(?WordLower:string, ?Sorted:list(atom), ?Candidate:string).

True if \verb|Candidate| is an anagram of the lower case string \verb|WordLower|
where \verb|Sorted| is assumed to be the sorted list of characters of
\verb|WordLower|, i.e., \break
\verb|msort_string_lower(Word, WordLower, Sorted)|.

\PL*/
is_anagram(WordLower, Sorted, Candidate) :-
    msort_string_lower(Candidate, CandidateLower, Sorted),
    \+ CandidateLower == WordLower.
/*PL

\Predicate anagram/3(+Word:string, +Candidates:list(string), -Anagrams:list(string)).

Filter elements \verb|Candidate| of \verb|Candidates| for which
\verb|is_anagram(Word, Candidate)| succeeds. True if \verb|Anagrams| contains
those elements.

\PL*/
anagram(Word, Candidates, Anagrams) :-
    msort_string_lower(Word, WordLower, Sorted),
    include(is_anagram(WordLower, Sorted), Candidates, Anagrams).
/*PL

\EndProlog*/
