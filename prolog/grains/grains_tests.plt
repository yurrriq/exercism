pending :-
    current_prolog_flag(argv, ['--all'|_]).
pending :-
    write('\nA TEST IS PENDING!\n'),
    fail.

:- begin_tests(grains).

    test(first, condition(true)) :-
        square(1, Result), Result == 1.

    test(second, condition(true)) :-
        square(2, Result), Result == 2.

    test(third, condition(true)) :-
        square(3, Result), Result == 4.

    test(sixteen, condition(true)) :-
        square(16, Result), Result == 32768.

    test(twenty_three, condition(true)) :-
        square(23, Result), Result == 4194304.

    test(thirty_two, condition(true)) :-
        square(32, Result), Result == 2147483648.

    test(fifty_five, condition(true)) :-
        square(55, Result), Result == 18014398509481984.

    test(sixty_four, condition(true)) :-
        square(64, Result), Result == 9223372036854775808.

    test(total, condition(true)) :-
        total(Result), Result == 18446744073709551615.

    test(zero, [fail, condition(true)]) :-
        square(0, _).

    test(negative, [fail, condition(true)]) :-
        square(-1, _).

    test(off_board, [fail, condition(true)]) :-
        square(65, _).

:- end_tests(grains).
