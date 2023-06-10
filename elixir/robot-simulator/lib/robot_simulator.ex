defmodule RobotSimulator do
  @type robot() :: %{
          direction: direction(),
          position: position()
        }

  @type direction() :: :north | :east | :south | :west
  defguard is_direction(direction)
           when direction == :north or
                  direction == :east or
                  direction == :south or
                  direction == :west

  @doc """
  Return the robot's direction.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec direction(robot) :: direction()
  def direction(robot), do: robot[:direction]

  @type position() :: {integer(), integer()}
  defguard is_position(position)
           when tuple_size(position) == 2 and
                  is_integer(elem(position, 0)) and
                  is_integer(elem(position, 1))

  @doc """
  Return the robot's position.
  """
  @spec position(robot) :: position()
  def position(robot), do: robot[:position]

  @type bearing() :: ?L | ?R
  defguard is_bearing(bearing)
           when bearing == ?L or
                  bearing == ?R

  @type instruction() :: ?A | bearing()
  defguard is_instruction(instruction)
           when instruction == ?A or
                  is_bearing(instruction)

  @doc """
  Create a Robot Simulator given an initial direction and position.

  Valid directions are: `:north`, `:east`, `:south`, `:west`
  """
  @spec create(direction, position) :: robot() | {:error, String.t()}
  def create(direction \\ :north, position \\ {0, 0})

  def create(direction, position)
      when is_direction(direction) and is_position(position) do
    %{direction: direction, position: position}
  end

  def create(_, position) when is_position(position) do
    {:error, "invalid direction"}
  end

  def create(direction, _) when is_direction(direction) do
    {:error, "invalid position"}
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
