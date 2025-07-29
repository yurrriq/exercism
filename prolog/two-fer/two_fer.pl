:- module(two_fer, [two_fer/1, two_fer/2]).

two_fer(Dialogue) :-
    two_fer('you', Dialogue).

two_fer(Name, Dialogue) :-
    format(string(Dialogue), 'One for ~s, one for me.', [Name]).
