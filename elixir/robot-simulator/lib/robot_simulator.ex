defmodule RobotSimulator.Guards do
  @moduledoc """
  Guards for `RobotSimulator`.
  """

  @doc """
  Return `true` iff `term` is a `t:RobotSimulator.direction/0`.

  Allowed in guard tests. Inlined by the compiler.
  """
  defguard is_direction(term) when term in ~w(north east south west)a

  @doc """
  Return `true` iff `term` is a `t:RobotSimulator.position/0`.

  Allowed in guard tests. Inlined by the compiler.
  """
  defguard is_position(term)
           when is_tuple(term) and tuple_size(term) == 2 and
                  is_integer(elem(term, 0)) and is_integer(elem(term, 1))

  @doc """
  Return `true` iff `term` is a `t:RobotSimulator.bearing/0`.

  Allowed in guard tests. Inlined by the compiler.
  """
  defguard is_bearing(term) when term in [?L, ?R]

  @doc """
  Return `true` iff `term` is a `t:RobotSimulator.instruction/0`.

  Allowed in guard tests. Inlined by the compiler.
  """
  defguard is_instruction(term) when term == ?A or is_bearing(term)
end

defmodule RobotSimulator do
  @moduledoc """
  A robot simulator.
  """

  import RobotSimulator.Guards

  @typedoc """
  A robot has a `t:direction/0` and a `t:position/0`.
  """
  @type robot() :: %{
          direction: direction(),
          position: position()
        }

  @typedoc """
  One of `:north`, `:east`, `:south`, and `:west`.
  """
  @type direction() :: :north | :east | :south | :west

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(robot), do: robot[:direction]

  @typedoc """
  A 2-tuple of Cartesian coordinates.
  """
  @type position() :: {integer(), integer()}

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(robot), do: robot[:position]

  @typedoc """
  `?A` for advance or a `t:bearing/0`.
  """
  @type instruction() :: ?A | bearing()

  @typedoc """
  `?L` to turn left or `?R` to turn right.
  """
  @type bearing() :: ?L | ?R

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(direction \\ :north, position \\ {0, 0})

  def create(direction, _) when not is_direction(direction) do
    {:error, "invalid direction"}
  end

  def create(_, position) when not is_position(position) do
    {:error, "invalid position"}
  end

  def create(direction, position) do
    %{direction: direction, position: position}
  end

  @doc """
  Simulate the robot's movement given a string of instructions.

  Valid instructions are: "R" (turn right), "L", (turn left), and "A" (advance)
  """
  @spec simulate(robot, instructions :: String.t()) :: robot() | {:error, String.t()}
  def simulate(robot, <<?A, instructions::binary>>) do
    simulate(advance(robot), instructions)
  end

  def simulate(robot = %{direction: direction}, <<instruction, instructions::binary>>)
      when is_bearing(instruction) do
    simulate(%{robot | direction: turn(direction, instruction)}, instructions)
  end

  def simulate(robot, ""), do: robot
  def simulate(_, _), do: {:error, "invalid instruction"}

  @spec advance(robot()) :: robot()
  defp advance(robot = %{direction: :north, position: {x, y}}) do
    %{robot | position: {x, y + 1}}
  end

  defp advance(robot = %{direction: :east, position: {x, y}}) do
    %{robot | position: {x + 1, y}}
  end

  defp advance(robot = %{direction: :south, position: {x, y}}) do
    %{robot | position: {x, y - 1}}
  end

  defp advance(robot = %{direction: :west, position: {x, y}}) do
    %{robot | position: {x - 1, y}}
  end

  @spec turn(direction(), bearing()) :: direction()
  defp turn(:north, ?R), do: :east
  defp turn(:north, ?L), do: :west
  defp turn(:east, ?R), do: :south
  defp turn(:east, ?L), do: :north
  defp turn(:south, ?R), do: :west
  defp turn(:south, ?L), do: :east
  defp turn(:west, ?R), do: :north
  defp turn(:west, ?L), do: :south
end
