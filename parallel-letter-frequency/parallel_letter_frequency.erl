%%% ==================================================================
%%% @author Eric Bailey
%%% @copyright 2016 Eric Bailey
%%% @version 1.0.0
%%% @doc Computing letter frequency in parallel.
%%% See the <a href="overview-summary.html">overview</a> for more information.
%%% @end
%%% ==================================================================

-module(parallel_letter_frequency).

%% API
-export([dict/1]).

%%% ==================================================================
%%% Public API
%%% ==================================================================

%% @type frequencies(). Frequencies are a dictionary from `char()' to
%% `integer()'.
-type frequencies() :: dict:dict(char(), integer()).

%% @doc Given a list of strings, compute their combined letter
%% {@type frequencies()} in parallel and return the result.
-spec dict([string()]) -> frequencies().
dict(Strings) ->
  Pid = spawn(fun loop/0),
  lists:foreach(fun(String) -> Pid ! {string, String} end, Strings),
  Pid ! {done, self()},
  receive
    Freqs -> Freqs
  end.


%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% @doc Initialize a loop to compute letter {@type frequencies()}.
%% @equiv loop(dict:new())
-spec loop() -> frequencies().
loop() -> loop(dict:new()).

%% @doc Handle messages of the form, `` {string, String} | {done, From} ''.
%%
%% <dl>
%%   <dt>`{string, String}'</dt>
%%   <dd>
%%     Compute the letter {@link frequency/2. frequency} in `String',
%%     combining the results with the previous state, `Freqs'.
%%   </dd>
%%   <dt>`{done, From}'</dt>
%%   <dd>Send `Freqs' to `From' and exit the loop.</dd>
%% </dl>
%%
%% @see frequency/2
-spec loop(frequencies()) -> frequencies().
loop(Freqs) ->
  receive
    {string, String} ->
      loop(lists:foldl(fun frequency/2, Freqs, String));
    {done,   From}   ->
      From ! Freqs
  end.

%% @doc Given a character `Char', and previously computed
%% {@type frequencies()} `Freqs', increment the value of `Char'
%% in `Freqs' by one.
-spec frequency(char(), frequencies()) -> frequencies().
frequency(Char, Freqs) -> dict:update_counter(Char, 1, Freqs).
