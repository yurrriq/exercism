%%% ======================================================================
%%% @author Eric Bailey
%%% @doc Calculating the largest product of a
%%% series of consecutive digits of length `N'.
%%% @end
%%% ======================================================================

-module(largest_series_product).

-export([from_string/2]).

%% @type char_digit(). A character representing a digit, i.e. `$0..$9'.
-type char_digit() :: 48..57.

%% @type digit(). A single digit integer, i.e. `0..9'.
-type digit() :: 0..9.

%% @doc Given a `String' of digits, calculate the largest product
%% for a series of consecutive digits of length `Len'.
-spec from_string(String, Len) -> Product when
    String  :: [char_digit()],
    Len     :: non_neg_integer(),
    Product :: integer().
from_string(String, N) when length(String) >= N, N >= 0 ->
  max1(lists:map(fun product/1, slices(digits(String), N))).


%%% ======================================================================
%%% Digit translation
%%% ======================================================================

%% @doc Convert a string of {@link char_digit(). digits}
%% to a list of {@link digit(). digits}.
%% @see digit/1
-spec digits(String) -> Digits when
    String :: [char_digit()],
    Digits :: [digit()].
digits(String) -> lists:map(fun digit/1, String).

%% @doc Convert a {@type char_digit()} to the corresponding {@type digit()}.
-spec digit(Char) -> Digit when
    Char  :: char_digit(),
    Digit :: digit().
digit(C) when C >= $0, C =< $9 -> C - $0.


%%% ======================================================================
%%% List functions
%%% ======================================================================

%% @doc Drop the last `Len' elements of `List1'.
%% If the list is empty or `Len' exceeds the length of the list,
%% return the empty list.
-spec droplast(List1, Len) -> List2 when
    List1 :: [T],
    List2 :: [T],
    Len   :: non_neg_integer(),
    T     :: term().
droplast(List, L) when is_integer(L), is_list(List) ->
  lists:sublist(List, 1, max(0, length(List) - L)).

%% @doc Return the first element of `List' that compares greater than or equal
%% to all other elements of `List'. If `List' is empty, return `1'.
-spec max1(List) -> Max when
    List :: [T],
    Max  :: T,
    T    :: term().
max1([H|T])                    -> max1(T, H);
max1([])                       -> 1.

%% @hidden
max1([H|T], Max) when H > Max -> max1(T, H);
max1([_|T], Max)              -> max1(T, Max);
max1([],    Max)              -> Max.

%% @doc Return the product of the elements in `List'.
-spec product(List) -> number() when
    List :: [number()].
product(L)              -> product(L, 1).

%% @hidden
product([H|T], Product) -> product(T, Product * H);
product([], Product)    -> Product.

%% @doc Return every distinct `Len'-element sub-list of `Len'.
-spec slices(List, Len) -> Slices when
    List   :: [T],
    Slices :: [[T]],
    Len    :: non_neg_integer(),
    T      :: term().
slices(L, N) -> [lists:sublist(L2, N) || L2 <- droplast(tails(L), N)].

%% @doc Return all tails of `List', longest first. For example,
%% ```
%% tails("abc") =:= ["abc","bc","c",[]].
%% '''
-spec tails(List) -> Tails when
    List  :: [T],
    Tails :: [[T]],
    T     :: term().
tails(List) -> [lists:nthtail(N, List) || N <- lists:seq(0, length(List))].


%% Local Variables:
%% compile-command: "make test"
%% End:
