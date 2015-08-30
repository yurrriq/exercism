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
%% Perform a right-associative fold, keeping track of the index
%% as well as the `Total'.
%%
%% If any character other than `$0' or `$1' is found, return `0'.
%% Otherwise, at each step, add one to `Index'. If the character is `$0',
%% leave `Total' unchanged otherwise when it is `$1', add `2^Index' to it.
%% That is, each step returns `{Index + 1, NewTotal}'.
%%
%% If no invalid charcters are encountered, the final `NewTotal' is returned.
-spec to_decimal(string()) -> integer().
to_decimal(String) ->
  try
    element(2, lists:foldr(fun to_decimal/2, {0, 0}, String))
  catch
    _:_ -> 0
  end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec to_decimal(48 | 49, {integer(), integer()}) -> {integer(), integer()}.
to_decimal($0, {Index, Total}) ->
  {Index + 1, Total};
to_decimal($1, {Index, Total}) ->
  {Index + 1, Total + trunc(math:pow(2, Index))}.
