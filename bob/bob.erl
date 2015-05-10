-module(bob).
-export([response_for/1]).

response_for(Prompt) ->
    case Prompt of
        [] -> "Fine. Be that way!";
        _  ->
            Last      = lists:last(Prompt),
            SomeUpper = lists:any(fun (C) -> C >= $A andalso C =< $Z end, Prompt),
            Trimmed   = tl(re:replace(Prompt, "\\s*", "")),
            Yelled    = string:to_upper(Prompt),
            case {Last, SomeUpper, Trimmed, Prompt} of
                {_, true, _, Yelled} -> "Whoa, chill out!";
                {$?, _, _, _}        -> "Sure.";
                {_, _, [], _}        -> "Fine. Be that way!";
                {_, _, _, _}         -> "Whatever."
            end
    end.
