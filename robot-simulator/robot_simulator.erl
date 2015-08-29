%% =============================================================================
%% @author Eric Bailey
%% @doc Simulating the movement of robots.
%% See the <a href="overview-summary.html">overview</a> for more information.
%% @end
%% =============================================================================

-module(robot_simulator).

-behaviour(gen_server).

%% Creation and read-only API
-export([create/0, direction/1, position/1]).

%% Mutating API
-export([place/3, control/2, advance/1, left/1, right/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% robot record with default values as undefined
-record(robot, {direction = undefined              :: direction(),
                position  = {undefined, undefined} :: coordinates()}).

%% @type direction(). A direction is `north', `east', `south', `west' or
%% `undefined'.
-type direction() :: 'north' | 'east' | 'south' | 'west' | 'undefined'.

%% @type bearing(). A bearing is either `left' or `right'.
-type bearing() :: 'left' | 'right'.

%% @type coordinates(). A set of coordinates is a pair of `X' and `Y' positions,
%% e.g. `{3, 8}'.
-type coordinates() :: {integer(), integer()} | {'undefined', 'undefined'}.

%% @doc Create a new robot and return its pid.
-spec create() -> pid().
create() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  Pid.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               READ-ONLY API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a robot pid, synchronously get and return its current direction.
%%
%% @see left/1
%% @see right/1
-spec direction(pid()) -> bearing().
direction(Pid) -> get_property(Pid, direction).

%% @doc Given a robot pid, synchronously get and return its current position.
%%
%% @see advance/1
-spec position(pid()) -> coordinates().
position(Pid)  -> get_property(Pid, position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                MUTATING API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Given a robot pid, a direction and a position, asynchronously set
%% its direction and position to the given values. Return `ok' immediately.
%%
%% @see direction/1
%% @see position/1
-spec place(pid(), bearing(), coordinates()) -> 'ok'.
place(Pid, Direction, Position) ->
  gen_server:cast(Pid, {place, Direction, Position}).

%% @doc Given a robot pid and a string of instructions, for each letter in
%% `Instructions', perform one of the following actions:
%% <dl>
%%   <dt>`$A'</dt>
%%   <dd>{@link advance/1. advance}</dd>
%%   <dt>`$L'</dt>
%%   <dd>{@link left/1. turn left}</dd>
%%   <dt>`$R'</dt>
%%   <dd>{@link right/1. turn right}</dd>
%%   <dt>`_'</dt>
%%   <dd>`noop'</dd>
%% </dl>
%%
%% @see advance/1
%% @see left/1
%% @see right/1
-spec control(pid(), string()) -> 'ok'.
control(Pid, Instructions) ->
  lists:foreach(fun ($A) -> advance(Pid);
                    ($L) -> left(Pid);
                    ($R) -> right(Pid);
                    (_)  -> noop
                end,
                Instructions).

%% @doc Given a robot pid, asynchronously advance its position one unit, based
%% on its current direction and position. Return `ok' immediately.
%%
%% @see direction/1
%% @see position/1
-spec advance(pid()) -> 'ok'.
advance(Pid) -> gen_server:cast(Pid, advance).

%% @doc Given a robot pid, asynchronously change its direction by turning left.
%% Return `ok' immediately.
%%
%% @see direction/1
%% @see right/1
-spec left(pid()) -> 'ok'.
left(Pid)    -> gen_server:cast(Pid, {turn, left}).

%% @doc Given a robot pid, asynchronously change its direction by turning right.
%% Return `ok' immediately.
%%
%% @see direction/1
%% @see left/1
-spec right(pid()) -> 'ok'.
right(Pid)   -> gen_server:cast(Pid, {turn, right}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                PRIVATE API                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This is a hack for the tests. In real life, I'd return {ok, Value} and not
%% define this helper function at all.
get_property(Pid, Property) ->
  try
    {ok, Value} = gen_server:call(Pid, Property),
    Value
  catch
    _Class:_Exception -> error
  end.

-spec advance(direction(), coordinates()) -> coordinates().
advance(north, {X, Y}) -> {X,     Y + 1};
advance(east,  {X, Y}) -> {X + 1, Y};
advance(south, {X, Y}) -> {X,     Y - 1};
advance(west,  {X, Y}) -> {X - 1, Y}.

-spec turn(direction(), bearing()) -> direction().
turn(north, right) -> east;
turn(north, left)  -> west;
turn(east,  right) -> south;
turn(east,  left)  -> north;
turn(south, right) -> west;
turn(south, left)  -> east;
turn(west,  right) -> north;
turn(west,  left)  -> south.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                            GEN_SERVER CALLBACKS                            %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
init([Direction, Position]) ->
  {ok, #robot{direction = Direction,
              position  = Position}};
init([]) ->
  {ok, #robot{}}.

%% @private
handle_call(direction, _From, Robot) ->
  #robot{direction = Direction} = Robot,
  {reply, {ok, Direction}, Robot};
handle_call(position, _From, Robot) ->
  #robot{position = Position} = Robot,
  {reply, {ok, Position}, Robot}.

%% @private
handle_cast(advance, Robot) ->
  #robot{direction = Direction,
         position  = Position} = Robot,
  {noreply, Robot#robot{position = advance(Direction, Position)}};
handle_cast({place, Direction, Position}, Robot) ->
  {noreply, Robot#robot{direction = Direction,
                        position  = Position}};
handle_cast({turn, Turn}, Robot) ->
  #robot{direction = Direction} = Robot,
  {noreply, Robot#robot{direction = turn(Direction, Turn)}}.

%% @private
handle_info(timeout, State)         -> {noreply, State}.

%% @private
terminate(_Reason, _State)          -> ok.

%% @private
code_change(_OldVsn, State, _Extra) -> {ok, State}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                EMACS CONFIG                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local Variables:
%% erlang-indent-level: 2
%% End:
