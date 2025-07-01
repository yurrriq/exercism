:- encoding(utf8).

pending :-
    current_prolog_flag(argv, ['--all'|_]).
pending :-
    write('\nA TEST IS PENDING!\n'),
    fail.

:- begin_tests(anagram).

    test(no_matches, condition(true)) :-
        anagram("diaper", [ "hello", "world", "zombies", "pants"], Anagrams),
        is_list(Anagrams).

    test(detects_simple_anagram, condition(true)) :-
        anagram("ant", ["tan", "stand", "at"], Anagrams),
        Anagrams == ["tan"].

    test(does_not_detect_false_positives, condition(true)) :-
        anagram("galea", ["eagle"], Anagrams),
        Anagrams == [].

    test(detects_multiple_anagrams, condition(true)) :-
        anagram("master", ["stream", "pigeon", "maters"], Anagrams),
        Anagrams == ["stream", "maters"].

    test(does_not_detect_anagram_subsets, condition(true)) :-
        anagram("good", ["dog", "goody"], Anagrams),
        Anagrams == [].

    test(detects_anagram, condition(true)) :-
        anagram("listen", ["enlists", "google", "inlets", "banana"], Anagrams),
        Anagrams == ["inlets"].

    test(detects_multiple_anagrams, condition(true)) :-
        anagram("allergy", ["gallery", "ballerina", "regally", "clergy", "largely", "leading"], Anagrams),
        Anagrams == ["gallery", "regally", "largely"].

    test(does_not_detect_identical_words, condition(true)) :-
        anagram("corn", ["corn", "dark", "Corn", "rank", "CORN", "cron", "park"], Anagrams),
        Anagrams == ["cron"].

    test(does_not_detect_nonanagrams_with_identical_checksum, condition(true)) :-
        anagram("mass", ["last"], Anagrams),
        Anagrams == [].

    test(detects_anagrams_case_insensitively, condition(true)) :-
        anagram("Orchestra", ["cashregister", "Carthorse", "radishes"], Anagrams),
        Anagrams == ["Carthorse"].

    test(detects_anagrams_using_case_insensitive_subject, condition(true)) :-
        anagram("Orchestra", ["cashregister", "carthorse", "radishes"], Anagrams),
        Anagrams == ["carthorse"].

    test(detects_anagrams_using_case_insensitive_possible_matches, condition(true)) :-
        anagram("orchestra", ["cashregister", "Carthorse", "radishes"], Anagrams),
        Anagrams == ["Carthorse"].

    test(does_not_detect_a_word_as_its_own_anagram, condition(true)) :-
        anagram("banana", ["Banana"], Anagrams),
        Anagrams == [].

    test(does_not_detect_a_anagram_if_the_original_word_is_repeated, condition(true)) :-
        anagram("go", ["go Go GO"], Anagrams),
        Anagrams == [].

    test(anagrams_must_use_all_letters_exactly_once, condition(true)) :-
        anagram("tapper", ["patter"], Anagrams),
        Anagrams == [].

    test(detects_unicode_anagrams, condition(true)) :-
        anagram("ΑΒΓ", ["ΒΓΑ", "ΒΓΔ", "γβα"], Anagrams),
        Anagrams == ["ΒΓΑ", "γβα"].

    test(eliminates_misleading_unicode_anagrams, condition(true)) :-
        anagram("ΑΒΓ", ["ABΓ"], Anagrams),
        Anagrams == [].

    test(capital_word_is_not_own_anagram, condition(true)) :-
        anagram("BANANA", ["Banana"], Anagrams),
        Anagrams == [].

:- end_tests(anagram).
