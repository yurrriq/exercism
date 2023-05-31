%%% ===================================================== [ roman_numerals.erl ]
%%% @doc Convert Arabic numbers to Roman numerals.
%%% @author Eric Bailey
%%% @end
%%% ==================================================================== [ EOH ]

-module(roman_numerals).

%% Public API.
-export([numerals/1, test_version/0]).

%% Types.
-export_types([arabic/0, conversion/0]).

%%% ================================================================== [ Types ]

-type arabic() ::
    1000
    | 900
    | 500
    | 400
    | 100
    | 90
    | 50
    | 40
    | 10
    | 9
    | 5
    | 4
    | 1.

-type conversion() :: {arabic(), string()}.

%%% ============================================================= [ Public API ]

%% @doc Given an Arabic `Number' return its Roman numerical representation.
-spec numerals(non_neg_integer()) -> string().
numerals(Number) ->
    do_numerals(conversions(), [], Number).

%% @hidden
-spec test_version() -> non_neg_integer().
test_version() ->
    1.

%%% ========================================================== [ Private Parts ]

-spec do_numerals([conversion()], iolist(), non_neg_integer()) -> string().
do_numerals([], Numerals, _Number) ->
    lists:flatten(Numerals);
do_numerals([{Arabic, Roman} | Conversions], Numerals, Number) when
    Number >= Arabic
->
    {Quotient, Remainder} = div_rem(Number, Arabic),
    NewNumerals = lists:duplicate(Quotient, Roman),
    do_numerals(Conversions, [Numerals | NewNumerals], Remainder);
do_numerals([_Conversion | Conversions], Numerals, Number) ->
    do_numerals(Conversions, Numerals, Number).

-spec div_rem(Numerator, Denominator) -> {Quotient, Remainder} when
    Numerator :: non_neg_integer(),
    Denominator :: pos_integer(),
    Quotient :: non_neg_integer(),
    Remainder :: non_neg_integer().
div_rem(Numerator, Denominator) ->
    Quotient = Numerator div Denominator,
    Remainder = Numerator rem Denominator,
    {Quotient, Remainder}.

-spec conversions() -> [conversion()].
conversions() ->
    [
        {1000, "M"},
        {900, "CM"},
        {500, "D"},
        {400, "CD"},
        {100, "C"},
        {90, "XC"},
        {50, "L"},
        {40, "XL"},
        {10, "X"},
        {9, "IX"},
        {5, "V"},
        {4, "IV"},
        {1, "I"}
    ].

%%% ==================================================================== [ EOF ]
