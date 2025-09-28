:- module(scrabble_score, [score/2]).
:- set_prolog_flag(double_quotes, chars).

%!  score(+Word:string, -Score:int) is det.
%
%   Calculate the Scrabble =Score= for a given =Word=.
score(Word, Score) :-
    string_upper(Word, WordUpper),
    string_chars(WordUpper, Letters),
    maplist(char_score, Letters, LetterScores),
    sum_list(LetterScores, Score).

%!  char_score(+Word:string, -Score:int) is det.
%
%   Calculate the Scrabble =Score= for a given =Letter=.
char_score(Letter,  1) :- member(Letter, "AEIOULNRST"), !.
char_score(Letter,  2) :- member(Letter, "DG"), !.
char_score(Letter,  3) :- member(Letter, "BCMP"), !.
char_score(Letter,  4) :- member(Letter, "FHVWY"), !.
char_score('K',     5) :- !.
char_score(Letter,  8) :- member(Letter, "JX"), !.
char_score(Letter, 10) :- member(Letter, "QZ"), !.
char_score(_Letter, 0) :- !.
