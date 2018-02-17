-module(phone).

-export([areacode/1, number/1, pretty_print/1]).

-define(BAD_NUMBER, "0000000000").

areacode(String) -> string:substr(number(String), 1, 3).

number(String) ->
  Digits = lists:filter(fun (C) -> $0 =< C andalso C =< $9 end, String),
  case length(Digits) of
    10 -> Digits;
    11 -> apply(fun ([$1|Rest]) -> Rest; (_) -> ?BAD_NUMBER end, [Digits]);
    _ -> ?BAD_NUMBER
  end.

pretty_print(String) ->
  List = tuple_to_list(parts(number(String))),
  lists:flatten(io_lib:format("(~s) ~s-~s", List)).

parts(String) ->
  apply(fun ({AreaCode, T}) ->
            erlang:insert_element(1, lists:split(3, T), AreaCode)
        end,
        [lists:split(3, String)]).


%% Local Variables:
%% erlang-indent-level: 2
%% End:
