%%% @author Eric Bailey
%%% @doc The Luhn formula.
%%%
%%% @reference <a href="https://www.youtube.com/watch?v=Q-VNe3piTw4">"4312" by Tyvek</a>
%%% @reference <a href="http://exercism.io/exercises/erlang/luhn/readme">README</a>
%%% @reference <a href="http://exercism.io/exercises/erlang/luhn">Test suite</a>
%%% @reference <a href="https://github.com/yurrriq/exercism/blob/erlang/luhn/src/luhn.erl">Source</a>

-module(luhn).
-author("Eric Bailey").

%% API
-export([valid/1, create/1]).

%% Types
-export_type([digit/0, valid_char/0]).

%% Property tests
-ifdef(TEST).
-compile(export_all).

-import(lists, [droplast/1, foldr/3, last/1]).

-include_lib("proper/include/proper.hrl").
-endif.

%%%===================================================================
%%% Types
%%%===================================================================

%% @type digit(). A digit is a single decimal digit.
-type digit() :: 0..9.

%% @type valid_char(). A valid char is one of `"0123456789 "'.
-type valid_char() :: 48..57 | 32.

%% @type parity(). Parity is either `even' or `odd'.
-type parity() :: even | odd.

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Given a `String', return `true' if it represents a valid number,
%% per the Luhn formula, otherwise `false'.
-spec valid(String :: string()) -> Valid :: boolean().
valid(S) -> 0 =:= rem10(do_luhn(S, odd)).

%% @doc Given a `String', calculate its check digit
%% and return it appended to `String'.
-spec create(String :: string()) -> Result :: string().
create(S) -> S ++ [$0 + rem10(10 - do_luhn(S, even))].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Equivalent to {@link check_digit/1}`(P, '{@link do_luhn/3}`(S, 0, 0)'.
-spec do_luhn(String :: string(), Parity :: parity()) -> CheckDigit :: digit().
do_luhn(S, P) -> check_digit(P, do_luhn(S, 0, 0)).

%% @doc Given a `String', `OddAcc' and `EvenAcc', return check digits
%% for both `odd' and `even' parity, as a tuple.
%% @see check_digit/1
%% @see do_luhn/2
-spec do_luhn(String, OddAcc, EvenAcc) -> {OddDigit, EvenDigit} when
    String :: string(),
    OddAcc :: non_neg_integer(),
    EvenAcc :: non_neg_integer(),
    OddDigit :: digit(),
    EvenDigit :: digit().
do_luhn([C | Cs], O, E) when $0 =< C, C =< $9 ->
    C0 = C - $0,
    F = fun(P) ->
        (fun
            (X) when X > 9 -> X - 9;
            (X) -> X
        end)(
            parity(P) * C0
        )
    end,
    do_luhn(Cs, E + F(odd), O + F(even));
do_luhn([_ | Cs], O, E) ->
    do_luhn(Cs, O, E);
do_luhn([], O, E) ->
    {O, E}.

%% @doc Return the numeric value of a given {@link partity/0. parity} `P',
%% i.e. `1' for `odd' and `2' for `even'.
-spec parity(P :: parity()) -> K :: 1 | 2.
parity(odd) -> 1;
parity(even) -> 2.

%% @doc Given a `Parity' and an `{OddAcc,EvenAcc}=OddEvenTuple',
%% return the appropriate `CheckDigit'.
%% @see do_luhn/3
-spec check_digit(Parity, OddEvenTuple) -> CheckDigit when
    Parity :: parity(),
    OddEvenTuple :: {non_neg_integer(), non_neg_integer()},
    CheckDigit :: digit().
check_digit(P, OE) -> rem10(element(parity(P), OE)).

%% @doc Return `X rem 10'.
-spec rem10(non_neg_integer()) -> digit().
rem10(X) -> X rem 10.

%%%===================================================================
%%% Property tests
%%%
%%%     rebar3 proper -d src -m luhn -n 10000
%%%
%%%===================================================================

-ifdef(TEST).
%%% Properties

%% @doc ∀ `Ds' ∈ {@link digits/0. digits}, {@link valid1/1. valid1}`(Ds)'
-spec prop_valid() -> proper:forall_clause().
prop_valid() -> ?FORALL(Ds, digits(), valid1(Ds)).

%% @doc ∀ `Ds' ∈ {@link digits/0. digits}, {@link valid/1. valid}`('
%% {@link create/1. create}`('{@link to_string/1. to_string}`(Ds)))'
-spec prop_create() -> proper:forall_clause().
prop_create() -> ?FORALL(Ds, digits(), valid(create(to_string(Ds)))).

%%% Types

%% @doc A PropEr type generator for a list of {@link digit(). digit}s,
%% such that `length(List) >= 3' and `hd(List) > 0'.
%% @see pos_digit/0
-spec digits() -> proper_types:raw_type().
digits() -> [pos_digit(), digit(), digit() | list(digit())].

%% @doc A PropEr type generator for a digit `0..9'.
%% @see digits/0
%% @see pos_digit/0
-spec digit() -> proper_types:type().
digit() -> integer(0, 9).

%% @doc A PropEr type generator for a digit `1..9'.
%% @see digits/0
%% @see digit/0
-spec pos_digit() -> proper_types:type().
pos_digit() -> integer(1, 9).

%%% Helper functions

%% @doc Given a list of {@link digits/0. digits} `L',
%% if `length(L)' is odd, prepend `0', otherwise return `L'.
-spec maybe_pad([digit(), ...]) -> [digit(), ...].
maybe_pad(L) when 0 =:= length(L) rem 2 -> L;
maybe_pad(L) -> [0 | L].

%% @doc Given a list of {@link digit/0. digits}, return its reversed string
%% representation, e.g. `"4312" = to_string([2,1,3,4])'.
-spec to_string([digit(), ...]) -> [valid_char(), ...].
to_string(Ds) -> foldr(fun(D, Acc) -> [$0 + D | Acc] end, "", Ds).

%% @doc Return `true' iff {@link luhn:valid/1. luhn:valid/1}
%% returns the correct result for `L'.
%% @see prop_create/0
-spec valid1([digit(), ...]) -> boolean().
valid1(L) -> (0 =:= rem10(sum(maybe_pad(L)))) =:= valid(to_string(L)).

%% @doc Return the sum, per the Luhn formula, of `Digits'.
-spec sum(Digits :: [digit(), ...]) -> non_neg_integer().
sum(L) -> last(L) + do_sum(droplast(L), 0).

%% @doc Implementation of {@link sum/1}.
-spec do_sum([digit()], non_neg_integer()) -> non_neg_integer().
do_sum([A, B | T], Acc) -> do_sum(T, Acc + norm(2 * A) + B);
do_sum([A], Acc) -> Acc + norm(2 * A);
do_sum([], Acc) -> Acc.

%% @doc Given a non-negative integer `N', subtract nine if `N >= 10',
%% otherwise return `N'.
-spec norm(N :: non_neg_integer()) -> N1 :: digit().
norm(N) when N >= 10 -> N - 9;
norm(N) -> N.
-endif.
