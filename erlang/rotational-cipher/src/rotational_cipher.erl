%%% ================================================== [ rotational_cipher.erl ]
%%% @doc Rotational cipher.
%%% @author Eric Bailey
%%% @end
%%% ==================================================================== [ EOH ]

-module(rotational_cipher).

%% Public API.
-export([encrypt/2, decrypt/2]).

-export([test_version/0]).

%% Types.
-export_type([key/0]).

-type key() :: 0..26.

%% Macros.
-define(IN_RANGE(Num, Min, Max), (Min =< Num andalso Num =< Max)).

%%% ============================================================= [ Public API ]

%% @todo write docstring
-spec encrypt(string(), key()) -> string().
encrypt(String, Key) ->
    lists:map(rotate(Key), String).

%% @todo write docstring
-spec decrypt(string(), key()) -> string().
decrypt(String, Key) ->
    encrypt(String, 26 - Key).

%% @doc Return the test version, to comply with the testing procedure for the
%% Erlang track of Exercism.
-spec test_version() -> non_neg_integer().
test_version() ->
    1.

%%% ========================================================== [ Private Parts ]

-spec rotate(key()) -> fun((char()) -> char()).
rotate(Key) ->
    fun(Char) -> rotate(Char, Key) end.

-spec rotate(char(), key()) -> char().
rotate(Char, Key) when ?IN_RANGE(Char, $a, $z) ->
    rotate_letter($a, Char, Key);
rotate(Char, Key) when ?IN_RANGE(Char, $A, $Z) ->
    rotate_letter($A, Char, Key);
rotate(Char, _Key) ->
    Char.

-spec rotate_letter(char(), char(), key()) -> char().
rotate_letter(MinChar, Char, Key) ->
    MinChar + ((Char - MinChar + Key) rem 26).

%%% ==================================================================== [ EOF ]
