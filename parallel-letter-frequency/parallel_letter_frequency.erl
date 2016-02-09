-module(parallel_letter_frequency).

-export([dict/1]).

-type frequencies() :: dict:dict(char(), integer()).

-spec dict([string()]) -> frequencies().
dict(Strings) ->
  Pid = spawn(fun loop/0),
  lists:foreach(fun(String) -> Pid ! {string, String} end, Strings),
  Pid ! {done, self()},
  receive
    Freqs -> Freqs
  end.

-spec loop() -> frequencies().
loop() -> loop(dict:new()).

-spec loop(frequencies()) -> frequencies().
loop(Freqs) ->
  receive
    {string, String} ->
      loop(lists:foldl(fun frequency/2, Freqs, String));
    {done,   From}   ->
      From ! Freqs
  end.

-spec frequency(char(), frequencies()) -> frequencies().
frequency(Char, Dict) -> dict:update_counter(Char, 1, Dict).
