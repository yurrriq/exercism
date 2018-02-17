-module(bob).
-export([response_for/1]).

%% @type prompt(). A prompt is a string.
-type prompt() :: string().

%% @type response(). A response is a string.
-type response() :: string().

%% @type character(). A character is an integer.
-type character() :: integer().

%% @doc Given a prompt, return an appropriate response.
-spec response_for(prompt()) -> response().
response_for([]) -> "Fine. Be that way!";
response_for(Prompt) when is_list(Prompt) ->
  answer(Prompt,
         [ {fun is_yelled/1,    "Whoa, chill out!"}
         , {fun is_question/1,  "Sure."}
         , {fun is_silent/1,    "Fine. Be that way!"}]).

%% @doc Given a `Prompt' and a list of {`Predicate', `Response'},
%% return the first `Response' where `Predicate(Prompt)' holds.
%%
%% If no predicate holds, return `"Whatever"'.
-spec answer(prompt(), [{function(), response()}]) -> response().
answer(_, []) -> "Whatever.";
answer(Prompt, [{Pred, Response} | Rest]) ->
  case Pred(Prompt) of
    true  -> Response;
    false -> answer(Prompt, Rest)
  end.

%% @doc Given a prompt, return `true' iff it has at least one uppercase letter
%% and is equal to itself uppercased.
-spec is_yelled(prompt()) -> boolean().
is_yelled(S) -> lists:any(fun is_upper/1, S) andalso S =:= string:to_upper(S).

%% @doc Given a prompt, return `true' iff it ends with `$?'.
-spec is_question(prompt()) -> boolean().
is_question([]) -> false;
is_question(S)  -> lists:last(S) =:= $?.

%% @doc Given a prompt, return `true' iff it is empty or only whitespace.
-spec is_silent(prompt()) -> boolean().
is_silent(S) -> re:replace(S, "\\s*", "", [{return, list}]) =:= [].

%% @doc Given a character, return `true' iff it is an uppercase letter A-Z.
-spec is_upper(character()) -> boolean().
is_upper(C) when C >= $A, C =< $Z -> true;
is_upper(_) -> false.
