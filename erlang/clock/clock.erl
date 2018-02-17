-module(clock).

-export([create/2, is_equal/2, minutes_add/2, minutes_delete/2, to_string/1]).

-type clock() :: {hour(), minutes()}.

-type hour() :: integer().

-type minutes() :: integer().

-define(DAY_IN_MINUTES, 1440). % 24 hours * 60 minutes/hour

-spec create(hour(), minutes()) -> clock().
create(H, M) when 0 =< H, H < 24, 0 =< M, M < 60 -> {H, M}.

-spec to_string(clock()) -> string().
to_string({H, M}) -> lists:flatten(io_lib:format("~2.10.0b:~2.10.0b", [H, M])).

-spec is_equal(clock(), clock()) -> boolean().
is_equal(C, C) -> true;
is_equal(_, _) -> false.

-spec minutes_add(clock(), minutes()) -> clock().
minutes_add(C, M) -> from_minutes((to_minutes(C) + M) rem ?DAY_IN_MINUTES).

-spec minutes_delete(clock(), minutes()) -> clock().
minutes_delete(C, M) ->
  from_minutes((to_minutes(C) + ?DAY_IN_MINUTES - M) rem ?DAY_IN_MINUTES).

-spec from_minutes(minutes()) -> clock().
from_minutes(M1) ->
  M2 = M1 rem ?DAY_IN_MINUTES,
  {M2 div 60, M2 rem 60}.

-spec to_minutes(clock()) -> minutes().
to_minutes({H, M}) -> (H * 60) + M.
