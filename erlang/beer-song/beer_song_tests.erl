-module(beer_song_tests).
-include_lib("eunit/include/eunit.hrl").

% To run tests:
% erl -make
% erl -noshell -eval "eunit:test(beer_song, [verbose])" -s init stop

verse_test() ->
    compare_nested_lists(
        beer_song:verse(8),
        "8 bottles of beer on the wall, 8 bottles of beer.\n"
        "Take one down and pass it around, 7 bottles of beer on the wall.\n"
    ).

verse_0_test() ->
    compare_nested_lists(
        beer_song:verse(0),
        "No more bottles of beer on the wall, no more bottles of beer.\n"
        "Go to the store and buy some more, 99 bottles of beer on the wall.\n"
    ).

verse_1_test() ->
    compare_nested_lists(
        beer_song:verse(1),
        "1 bottle of beer on the wall, 1 bottle of beer.\n"
        "Take it down and pass it around, no more bottles of beer on the wall.\n"
    ).

verse_2_test() ->
    compare_nested_lists(
        beer_song:verse(2),
        "2 bottles of beer on the wall, 2 bottles of beer.\n"
        "Take one down and pass it around, 1 bottle of beer on the wall.\n"
    ).

singing_several_verses_test() ->
    compare_nested_lists(
        beer_song:sing(8, 6),
        "8 bottles of beer on the wall, 8 bottles of beer.\n"
        "Take one down and pass it around, 7 bottles of beer on the wall.\n\n"
        "7 bottles of beer on the wall, 7 bottles of beer.\n"
        "Take one down and pass it around, 6 bottles of beer on the wall.\n\n"
        "6 bottles of beer on the wall, 6 bottles of beer.\n"
        "Take one down and pass it around, 5 bottles of beer on the wall.\n\n"
    ).

sing_all_the_rest_of_the_verses_test() ->
    compare_nested_lists(
        beer_song:sing(3),
        "3 bottles of beer on the wall, 3 bottles of beer.\n"
        "Take one down and pass it around, 2 bottles of beer on the wall.\n\n"
        "2 bottles of beer on the wall, 2 bottles of beer.\n"
        "Take one down and pass it around, 1 bottle of beer on the wall.\n\n"
        "1 bottle of beer on the wall, 1 bottle of beer.\n"
        "Take it down and pass it around, no more bottles of beer on the wall.\n\n"
        "No more bottles of beer on the wall, no more bottles of beer.\n"
        "Go to the store and buy some more, 99 bottles of beer on the wall.\n\n"
    ).

compare_nested_lists(Response, Expected) ->
    ?assertEqual(lists:flatten(Expected), lists:flatten(Response)).
