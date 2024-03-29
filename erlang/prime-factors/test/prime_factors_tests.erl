%% Generated with 'testgen v0.2.0'
%% Revision 1 of the exercises generator was used
%% https://github.com/exercism/problem-specifications/raw/a88c5443c71a0b45ea7021574b976c8c7c89cd2e/exercises/prime-factors/canonical-data.json
%% This file is automatically generated from the exercises canonical data.

-module(prime_factors_tests).

-include_lib("erl_exercism/include/exercism.hrl").
-include_lib("eunit/include/eunit.hrl").

'1_no_factors_test_'() ->
    {"no factors",
        ?_assertEqual(
            lists:sort([]),
            lists:sort(prime_factors:factors(1))
        )}.

'2_prime_number_test_'() ->
    {"prime number",
        ?_assertEqual(
            lists:sort([2]),
            lists:sort(prime_factors:factors(2))
        )}.

'3_another_prime_number_test_'() ->
    {"another prime number",
        ?_assertEqual(
            lists:sort([3]),
            lists:sort(prime_factors:factors(3))
        )}.

'4_square_of_a_prime_test_'() ->
    {"square of a prime",
        ?_assertEqual(
            lists:sort([3, 3]),
            lists:sort(prime_factors:factors(9))
        )}.

'5_product_of_first_prime_test_'() ->
    {"product of first prime",
        ?_assertEqual(
            lists:sort([2, 2]),
            lists:sort(prime_factors:factors(4))
        )}.

'6_cube_of_a_prime_test_'() ->
    {"cube of a prime",
        ?_assertEqual(
            lists:sort([2, 2, 2]),
            lists:sort(prime_factors:factors(8))
        )}.

'7_product_of_second_prime_test_'() ->
    {"product of second prime",
        ?_assertEqual(
            lists:sort([3, 3, 3]),
            lists:sort(prime_factors:factors(27))
        )}.

'8_product_of_third_prime_test_'() ->
    {"product of third prime",
        ?_assertEqual(
            lists:sort([5, 5, 5, 5]),
            lists:sort(prime_factors:factors(625))
        )}.

'9_product_of_first_and_second_prime_test_'() ->
    {"product of first and second prime",
        ?_assertEqual(
            lists:sort([2, 3]),
            lists:sort(prime_factors:factors(6))
        )}.

'10_product_of_primes_and_non_primes_test_'() ->
    {"product of primes and non-primes",
        ?_assertEqual(
            lists:sort([2, 2, 3]),
            lists:sort(prime_factors:factors(12))
        )}.

'11_product_of_primes_test_'() ->
    {"product of primes",
        ?_assertEqual(
            lists:sort([5, 17, 23, 461]),
            lists:sort(prime_factors:factors(901255))
        )}.

'12_factors_include_a_large_prime_test_'() ->
    {"factors include a large prime",
        ?_assertEqual(
            lists:sort([11, 9539, 894119]),
            lists:sort(prime_factors:factors(93819012551))
        )}.
