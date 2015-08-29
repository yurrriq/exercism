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
-record(robot, {direction = undefined,
                position  = {undefined, undefined}}).


create() ->
  {ok, Pid} = gen_server:start_link(?MODULE, [], []),
  Pid.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               READ-ONLY API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

direction(Pid) -> get_property(Pid, direction).

position(Pid)  -> get_property(Pid, position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                MUTATING API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

place(Pid, Direction, Position) ->
  gen_server:cast(Pid, {place, Direction, Position}).

control(Pid, Instructions) ->
  lists:foreach(fun ($A) -> advance(Pid);
                    ($L) -> left(Pid);
                    ($R) -> right(Pid);
                    (_)  -> noop
                end,
                Instructions).

advance(Pid) -> gen_server:cast(Pid, advance).

left(Pid)    -> gen_server:cast(Pid, {turn, left}).

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

advance(north, {X, Y}) -> {X,     Y + 1};
advance(east,  {X, Y}) -> {X + 1, Y};
advance(south, {X, Y}) -> {X,     Y - 1};
advance(west,  {X, Y}) -> {X - 1, Y}.

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

init([Direction, Position]) ->
  {ok, #robot{direction = Direction,
              position  = Position}};
init([]) ->
  {ok, #robot{}}.

handle_call(direction, _From, Robot) ->
  #robot{direction = Direction} = Robot,
  {reply, {ok, Direction}, Robot};
handle_call(position, _From, Robot) ->
  #robot{position = Position} = Robot,
  {reply, {ok, Position}, Robot}.

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

handle_info(timeout, State)         -> {noreply, State}.

terminate(_Reason, _State)          -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                EMACS CONFIG                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local Variables:
%% erlang-indent-level: 2
%% End:
