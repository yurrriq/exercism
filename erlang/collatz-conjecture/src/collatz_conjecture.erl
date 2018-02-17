%%% ================================================= [ collatz_conjecture.erl ]
%%% @doc Collatz Conjecture.
%%% Compute the number of steps required, as per the Collatz Conjecture,
%%% to reach 1, given a strictly positive number `Number'.
%%% @end
%%%
%%% @author Eric Bailey
%%% @copyright 2017 Eric Bailey
%%% @version 0.0.1
%%% @end
%%% ==================================================================== [ EOH ]
-module(collatz_conjecture).


%%% Public API.
-export([steps/1, test_version/0]).


%%% Macros.
-define(IS_EVEN(N), (0 == N band 1)).
-define(DEFAULT_ERROR, {error, "Only strictly positive numbers are allowed"}).


%%% ============================================================= [ Public API ]

%% @doc Compute the number of steps required, as per the Collatz Conjecture,
%% to reach 1, given a strictly positive number `Number'.
-spec steps(Number :: pos_integer()) -> Steps :: non_neg_integer();
           (any()) -> {error, Reason :: string()}.

steps(1) ->
    0;
steps(Number) when 0 < Number ->
    do_steps(Number, 0);
steps(_BadNumber) ->
    ?DEFAULT_ERROR.


%% @doc Return the test version, to comply with the testing procedure for the
%% Erlang track of Exercism.
test_version() ->
    1.


%%% ========================================================== [ Private Parts ]

%% @doc Internal implementation of {@link steps/1}.
-spec do_steps(Number, Step) -> Steps when
      Number :: pos_integer(),
      Step   :: non_neg_integer(),
      Steps  :: non_neg_integer().
%% If we've reached 1, then we're done.
do_steps(1, Steps) ->
    Steps;
%% If `Number' is even, divide it by 2.
do_steps(Number, Step) when ?IS_EVEN(Number) ->
    do_steps(Number div 2, inc(Step));
%% Otherwise, `Number' is odd, so multiply it by 3 and add 1.
do_steps(Number, Step) ->
    do_steps(inc(3 * Number), inc(Step)).


%% @doc Return a number on greater than a given `Number'.
-spec inc(number()) -> number().
inc(Number) ->
    Number + 1.

%%% ==================================================================== [ EOF ]
