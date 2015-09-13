-module(word_count).

-export([count/1]).

count(String) ->
  %% So lame that I have to remove_empty. Come on, re:split/3..
  %% http://erlang.org/pipermail/erlang-questions/2013-December/076202.html
  Words = remove_empty(re:split(String, "[^0-9A-Za-z]+",[{return,list},trim])),
  F     = fun(W, D) -> dict:update_counter(string:to_lower(W), 1, D) end,
  lists:foldl(F, dict:new(), Words).

remove_empty(List) ->
  Pred = fun(X) ->
             case X of
               [] -> false;
               _  -> true
             end
         end,
  lists:filter(Pred, List).
