-module(robot_simulator).

%% Creation and read-only API
-export([create/0, direction/1, position/1]).

%% Mutating API
-export([place/3, control/2, advance/1, left/1, right/1]).


create() -> erlang:spawn(fun() -> loop(undefined, {undefined, undefined}) end).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               READ-ONLY API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

direction(Robot) -> get_property(Robot, direction).

position(Robot) -> get_property(Robot, position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                MUTATING API                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

place(Robot, Direction, Position) -> Robot ! {place, Direction, Position}.

control(Robot, Instructions) ->
  lists:foreach(fun ($A) -> advance(Robot);
                    ($L) -> left(Robot);
                    ($R) -> right(Robot);
                    (_)  -> noop
                end,
                Instructions).

advance(Robot) -> Robot ! advance.

left(Robot)    -> Robot ! {turn, left}.

right(Robot)   -> Robot ! {turn, right}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                PRIVATE API                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_property(Robot, Property) ->
  Robot ! {Property, erlang:self()},
  receive
    {Property, Value} -> Value
  end.

loop(Direction, Position) ->
  receive
    advance ->
      loop(Direction, advance(Direction, Position));
    {place, NewDirection, NewPosition} ->
      loop(NewDirection, NewPosition);
    {turn, Turn} ->
      loop(turn(Direction, Turn), Position);
    {direction, Robot} ->
      Robot ! {direction, Direction},
      loop(Direction, Position);
    {position, Robot} ->
      Robot ! {position, Position},
      loop(Direction, Position)
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
%%                                EMACS CONFIG                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Local Variables:
%% erlang-indent-level: 2
%% End:
