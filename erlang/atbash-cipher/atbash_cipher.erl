-module(atbash_cipher).

-export([encode/1, decode/1]).

encode(String) -> transcode(String).

decode(String) -> transcode(String).

transcode(String) -> lists:map(fun cipher/1, String).

cipher(Char) when $A =< Char, Char =< $Z -> $Z - (Char - $A);
cipher(Char) when $a =< Char, Char =< $z -> $z - (Char - $a);
cipher(Char) -> Char.

%% Local Variables:
%% compile-command: "erlc *.erl; and erl -noshell -eval 'eunit:test(atbash_cipher_tests, [verbose])' -s init stop"
%% erlang-indent-level: 2
%% End:
