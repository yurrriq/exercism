%% Generated with 'testgen v0.2.0'
%% Revision 1 of the exercises generator was used
%% https://github.com/exercism/problem-specifications/raw/d137db179c6a760be6aa6e38ac0110e96be0b9ff/exercises/isbn-verifier/canonical-data.json
%% This file is automatically generated from the exercises canonical data.

-module(isbn_verifier_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").

'1_valid_isbn_test_'() ->
    {"valid isbn", ?_assert(isbn_verifier:is_valid("3-598-21508-8"))}.

'2_invalid_isbn_check_digit_test_'() ->
    {"invalid isbn check digit",
        ?_assertNot(isbn_verifier:is_valid("3-598-21508-9"))}.

'3_valid_isbn_with_a_check_digit_of_10_test_'() ->
    {"valid isbn with a check digit of 10",
        ?_assert(isbn_verifier:is_valid("3-598-21507-X"))}.

'4_check_digit_is_a_character_other_than_x_test_'() ->
    {"check digit is a character other than X",
        ?_assertNot(isbn_verifier:is_valid("3-598-21507-A"))}.

'5_invalid_character_in_isbn_is_not_treated_as_zero_test_'() ->
    {
        "invalid character in isbn is not treated "
        "as zero",
        ?_assertNot(isbn_verifier:is_valid("3-598-P1581-X"))
    }.

'6_x_is_only_valid_as_a_check_digit_test_'() ->
    {"X is only valid as a check digit",
        ?_assertNot(isbn_verifier:is_valid("3-598-2X507-9"))}.

'7_valid_isbn_without_separating_dashes_test_'() ->
    {"valid isbn without separating dashes",
        ?_assert(isbn_verifier:is_valid("3598215088"))}.

'8_isbn_without_separating_dashes_and_x_as_check_digit_test_'() ->
    {
        "isbn without separating dashes and X "
        "as check digit",
        ?_assert(isbn_verifier:is_valid("359821507X"))
    }.

'9_isbn_without_check_digit_and_dashes_test_'() ->
    {"isbn without check digit and dashes",
        ?_assertNot(isbn_verifier:is_valid("359821507"))}.

'10_too_long_isbn_and_no_dashes_test_'() ->
    {"too long isbn and no dashes",
        ?_assertNot(isbn_verifier:is_valid("3598215078X"))}.

'11_too_short_isbn_test_'() ->
    {"too short isbn", ?_assertNot(isbn_verifier:is_valid("00"))}.

'12_isbn_without_check_digit_test_'() ->
    {"isbn without check digit",
        ?_assertNot(isbn_verifier:is_valid("3-598-21507"))}.

'13_check_digit_of_x_should_not_be_used_for_0_test_'() ->
    {
        "check digit of X should not be used "
        "for 0",
        ?_assertNot(isbn_verifier:is_valid("3-598-21515-X"))
    }.

'14_empty_isbn_test_'() ->
    {"empty isbn", ?_assertNot(isbn_verifier:is_valid([]))}.

'15_input_is_9_characters_test_'() ->
    {"input is 9 characters", ?_assertNot(isbn_verifier:is_valid("134456729"))}.

'16_invalid_characters_are_not_ignored_after_checking_length_test_'() ->
    {
        "invalid characters are not ignored after "
        "checking length",
        ?_assertNot(isbn_verifier:is_valid("3132P34035"))
    }.

'17_invalid_characters_are_not_ignored_before_checking_length_test_'() ->
    {
        "invalid characters are not ignored before "
        "checking length",
        ?_assertNot(isbn_verifier:is_valid("3598P215088"))
    }.

'18_input_is_too_long_but_contains_a_valid_isbn_test_'() ->
    {
        "input is too long but contains a valid "
        "isbn",
        ?_assertNot(isbn_verifier:is_valid("98245726788"))
    }.
