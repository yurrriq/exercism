-module(zipper).

-export([
    empty/0,
    new_tree/3,
    value/1,
    left/1,
    right/1,
    set_value/2,
    set_left/2,
    set_right/2,
    from_tree/1,
    to_tree/1,
    up/1
]).

-export([test_version/0]).

empty() ->
    error(nyi).

new_tree(_Value, _Left, _Right) ->
    error(nyi).

value(_Node) ->
    error(nyi).

left(_Node) ->
    error(nyi).

right(_Node) ->
    error(nyi).

set_value(_Node, _Value) ->
    error(nyi).

set_left(_Node, _Left) ->
    error(nyi).

set_right(_Node, _Right) ->
    error(nyi).

from_tree(_Tree) ->
    error(nyi).

to_tree(_Tree) ->
    error(nyi).

up(_Zipper) ->
    error(nyi).

test_version() ->
    1.
