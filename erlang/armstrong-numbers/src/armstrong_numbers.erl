-module(armstrong_numbers).

-export([is_armstrong_number/1]).

%% @type char_digit(). A character representing a digit, i.e. `$0..$9'.
-type char_digit() :: 48..57.

%% @type digit(). A single digit integer, i.e. `0..9'.
-type digit() :: 0..9.

%% @doc Determine whether a given number is an Armstrong number.
-spec is_armstrong_number(number()) -> boolean().
is_armstrong_number(0) ->
    true;
is_armstrong_number(Number) ->
    String = integer_to_list(Number),
    NumDigits = trunc(math:log10(Number)) + 1,
    Fun = fun(CharDigit, Sum) ->
        Sum + int_power:exp(digit(CharDigit), NumDigits)
    end,
    Number == lists:foldl(Fun, 0, String).

%% @doc Convert a {@type char_digit()} to the corresponding {@type digit()}.
-spec digit(Char) -> Digit when
    Char :: char_digit(),
    Digit :: digit().
digit(C) when C >= $0, C =< $9 -> C - $0.
