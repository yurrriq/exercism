%%%% =================================================================== [ Bob ]
%%%% @doc Bob is a lackadaisical teenager.
-module(bob).

%% Public API
-export([response/1]).

%%%% =================================================================== [ EOH ]


%%%% ================================================================ [ Macros ]

-define(IF(Test, Then, Else), (case Test of true -> Then; false -> Else end)).


-define(RESPONSES,
	[ {fun is_silent/1,            "Fine. Be that way!"}
	, {fun is_forceful_question/1, "Calm down, I know what I'm doing!"}
	, {fun is_yelled/1,            "Whoa, chill out!"}
	, {fun is_question/1,          "Sure."}]).


%%%% ================================================================= [ Types ]

%% @type prompt(). A prompt is a string.
-type prompt() :: string().


%% @type response(). A response is a string.
-type response() :: string().


%%%% ============================================================ [ Public API ]

%% @doc Determine the appropriate response for a given `Prompt'.
-spec response(prompt()) -> response().
response([]) ->
    "Fine. Be that way!";
response(Prompt) when is_list(Prompt) ->
    answer(string:trim(Prompt), ?RESPONSES).


%%%% ========================================================= [ Private Parts ]

%% @doc Given a `Prompt' and a list of {`Predicate', `Response'},
%% return the first `Response' where `Predicate(Prompt)' holds.
%%
%% If no predicate holds, return `"Whatever"'.
-spec answer(prompt(), [{function(), response()}]) -> response().
answer(Prompt, [{Pred, Response} | Rest]) ->
    ?IF(Pred(Prompt), Response, answer(Prompt, Rest));
answer(_Prompt, []) ->
    "Whatever.".


%%%% ===================================================== [ Prompt Predicates ]

%% @doc Given a `Prompt', determine whether it is {@link is_yelled/1. yelled}
%% and also a {@link is_question/1. question}.
-spec is_forceful_question(prompt()) -> boolean().
is_forceful_question(Prompt) ->
    is_question(Prompt) andalso is_yelled(Prompt).


%% @doc Determine whether a given `Prompt' ends with `$?'.
-spec is_question(prompt()) -> boolean().
is_question([]) ->
    false;
is_question(Prompt) ->
    lists:last(Prompt) =:= $?.


%% @doc Determine whether a given `Prompt' is the empty string.
-spec is_silent(Prompt :: prompt()) -> boolean().
is_silent("")      -> true;
is_silent(_Prompt) -> false.


%% @doc Determine if a given `Character' is an uppercase letter, A-Z.
-spec is_upper(char()) -> boolean().
is_upper(Character) ->
    Character >= $A andalso Character =< $Z.


%% @doc Determine whether given `Prompt' has at least one uppercase letter,
%% and is equal to itself uppercased.
-spec is_yelled(prompt()) -> boolean().
is_yelled(Prompt) ->
    is_yelled(false, Prompt).

is_yelled(AnyUpper, [C | Cs]) ->
    case string:to_upper(C) of
	C -> is_yelled(AnyUpper orelse is_upper(C), Cs);
	_ -> false
    end;
is_yelled(AnyUpper, []) -> AnyUpper.


%%%% =================================================================== [ EOF ]
