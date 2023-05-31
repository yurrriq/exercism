%%% ====================================================== [ all_your_base.erl ]
%%% @doc All Your Base.
%%% Convert a number, represented as a sequence of digits in one base,
%%% to any other base.
%%% @end
%%%
%%% @author Eric Bailey
%%% @copyright 2017 Eric Bailey
%%% @version 0.0.1
%%% @end
%%% ==================================================================== [ EOH ]
-module(all_your_base).

%% Public API.
-export([convert/3, test_version/0]).

%% Macros.
-define(IS_VALID_BASE(B), (is_integer(B) andalso 2 =< B)).

%% Types.
-export_type([base_error/0, digit_error/0, error_tuple/0, valid_base/0]).

-type base_error() ::
    invalid_dst_base
    | invalid_src_base.

-type digit_error() ::
    negative
    | not_in_base.

-type error_tuple() :: {error, base_error() | digit_error()}.

% but actually `2..'
-type valid_base() :: pos_integer().

%%% ============================================================= [ Public API ]

%% @doc Convert a list of digits in `SourceBase' to `DestinationBase'.
-spec convert
    (InDigits, valid_base(), valid_base()) -> {ok, OutDigits} when
        InDigits :: [non_neg_integer()],
        OutDigits :: [non_neg_integer()];
    (_, _, _) -> error_tuple().
convert(Digits, SourceBase, DestinationBase) ->
    case from_digits(SourceBase, Digits) of
        {ok, Number} -> to_digits(DestinationBase, Number);
        Error -> Error
    end.

%% @doc Return the test version, to comply with the testing procedure for the
%% Erlang track of Exercism.
test_version() ->
    1.

%%% ========================================================== [ Private Parts ]

%% @doc Convert the decimal `Number' to a list of base `Base' digits.
%% If `Base' is invalid, return `{error, invalid_dst_base}',
%% otherwise `{ok, Digits}'.
%% @see base_error()
to_digits(Base, Number) when ?IS_VALID_BASE(Base) ->
    do_to_digits(Base, Number, []);
to_digits(_Base, _Number) ->
    {error, invalid_dst_base}.

do_to_digits(_Base, 0, Digits) ->
    {ok, Digits};
do_to_digits(Base, Number, Digits) ->
    Quotient = Number div Base,
    Remainder = Number rem Base,
    do_to_digits(Base, Quotient, [Remainder | Digits]).

%% @doc Convert a list of base `Base' `Digits' into a (base 10) integer.
%%
%% If `Base' is invalid, return `{error, invalid_src_base}'.
%%
%% Or if any `Digit' in `Digits' is negative, return `{error, negative}'.
%%
%% Or if any `Digit' in `Digits' is not strictly less than `Base',
%% return `{error, not_in_base}'.
%%
%% Otherwise return `{ok, Integer}'.
from_digits(Base, Digits) when ?IS_VALID_BASE(Base) ->
    do_from_digits(Base, Digits, 0);
from_digits(_Base, _Digits) ->
    {error, invalid_src_base}.

do_from_digits(_Base, [], Number) ->
    {ok, Number};
do_from_digits(_Base, [Digit | _Digits], _Acc) when Digit < 0 ->
    {error, negative};
do_from_digits(Base, [Digit | _Digits], _Acc) when Digit >= Base ->
    {error, not_in_base};
do_from_digits(Base, [Digit | Digits], Acc) ->
    NewAcc = Digit + Acc * Base,
    do_from_digits(Base, Digits, NewAcc).

%%% ==================================================================== [ EOF ]
