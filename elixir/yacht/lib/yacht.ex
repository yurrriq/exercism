defmodule Yacht do
  @type category ::
          :ones
          | :twos
          | :threes
          | :fours
          | :fives
          | :sixes
          | :full_house
          | :four_of_a_kind
          | :little_straight
          | :big_straight
          | :choice
          | :yacht

  @doc """
  Calculate the score of 5 dice using the given category's scoring method.
  """
  @spec score(category :: category(), dice :: [integer]) :: integer
  def score(:ones, dice), do: pips(1, dice)
  def score(:twos, dice), do: pips(2, dice)
  def score(:threes, dice), do: pips(3, dice)
  def score(:fours, dice), do: pips(4, dice)
  def score(:fives, dice), do: pips(5, dice)
  def score(:sixes, dice), do: pips(6, dice)

  def score(:full_house, dice) do
    case group(dice) do
      [[_, _], [_, _, _]] -> Enum.sum(dice)
      [[_, _, _], [_, _]] -> Enum.sum(dice)
      _ -> 0
    end
  end

  def score(:four_of_a_kind, dice) do
    case group(dice) do
      [[x, _, _, _], [_]] -> 4 * x
      [[_], [x, _, _, _]] -> 4 * x
      [[x, _, _, _, _]] -> 4 * x
      _ -> 0
    end
  end

  def score(:little_straight, dice) do
    case Enum.sort(dice) do
      [1,2,3,4,5] -> 30
      _ -> 0
    end
  end

  def score(:big_straight, dice) do
    case Enum.sort(dice) do
      [2,3,4,5,6] -> 30
      _ -> 0
    end
  end

  def score(:choice, dice), do: Enum.sum(dice)

  def score(:yacht, [die, die, die, die, die]), do: 50
  def score(_, _), do: 0

  defp pips(n, dice) do
    dice
    |> Enum.filter(&(&1 == n))
    |> Enum.sum()
  end

  defp group(list) do
    list
    |> Enum.sort
    |> Enum.group_by(&Function.identity/1)
    |> Map.values
  end
end
