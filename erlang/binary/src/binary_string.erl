%%====================================================================
%% @author Eric Bailey
%% @copyright 2015 Eric Bailey
%% @version 0.1.0
%% @doc Converting string binary numbers to their decimal equivalents.
%% @end
%%====================================================================

-module(binary_string).

%% API exports
-export([to_decimal/1]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Given a string representing a binary number,
%% return its decimal equivalent.
%%
%% Perform a left-associative fold with an initial value of `0',
%% pattern matching on each character.
%%
%% If any character other than `$0' or `$1' is encountered, return `0'.
%%
%% Otherwise, at each step perform a left arithmetic shift by 1 on the previous
%% value. If the character is `$1', also add one to the result.
%%
%% NB: The bit-shifting algorithm works since `0 bsl 1 =:= 0' and
%% `N bsl 1 =:= N * 2'.
-spec to_decimal(string()) -> integer().
to_decimal(String) -> to_decimal(String, 0).

%%====================================================================
%% Internal functions
%%====================================================================

-spec to_decimal(string(), integer()) -> integer().
to_decimal([], D) -> D;
to_decimal([$0 | T], D) -> to_decimal(T, D bsl 1);
to_decimal([$1 | T], D) -> to_decimal(T, 1 + (D bsl 1));
to_decimal(_, _) -> 0.
