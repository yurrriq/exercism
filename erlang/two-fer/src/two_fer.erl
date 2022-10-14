-module(two_fer).

-export([two_fer/0, two_fer/1]).

-spec two_fer() -> Result :: string().
two_fer() ->
    "One for you, one for me.".

-spec two_fer(Name :: string()) -> Result :: string().
two_fer(Name) ->
    io_lib:format("One for ~s, one for me.", [Name]).
