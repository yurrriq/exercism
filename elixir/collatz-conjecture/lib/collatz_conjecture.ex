defmodule CollatzConjecture do
  import Integer, only: [is_even: 1]

  @doc """
  calc/1 takes an integer and returns the number of steps required to get the
  number to 1 when following the rules:
    - if number is odd, multiply with 3 and add 1
    - if number is even, divide by 2
  """

  # @spec calc(1) :: 0
  def calc(1), do: 0

  @spec calc(number) :: steps
        when number: pos_integer(),
             steps: non_neg_integer()
  def calc(number) when is_integer(number) and number > 0 do
    number
    |> Stream.iterate(&do_calc/1)
    |> Stream.take_while(&(&1 != 1))
    |> Enum.count()
  end

  @spec do_calc(pos_integer()) :: non_neg_integer()
  defp do_calc(n) when is_even(n), do: div(n, 2)
  defp do_calc(n), do: n * 3 + 1
end
