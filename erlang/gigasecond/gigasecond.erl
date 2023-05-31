%% =============================================================================
%% @author Eric Bailey
%% @copyright 2015 Eric Bailey
%% @version 0.1.0
%% @doc Calculating gigasecond anniversaries.
%% @end
%% =============================================================================

-module(gigasecond).

-export([from/1]).

%% A gigasecond is one billion (10**9) seconds.
-define(GIGASECOND, 1000000000).

%% @doc Given a `calendar:date()' or a `calendar:datetime()', return a
%% `calendar:datetime()' a gigasecond later.
-spec from(calendar:date() | calendar:datetime()) -> calendar:datetime().
from(Date = {_Y, _M, _D}) ->
    from({Date, {0, 0, 0}});
from(DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Seconds + ?GIGASECOND).
