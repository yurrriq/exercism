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
    do_calc(number, 0)
  end

  # @spec calc(any()) :: no_return()

  @spec do_calc(number, step) :: steps
        when number: pos_integer(),
             step: non_neg_integer(),
             steps: non_neg_integer()
  def do_calc(1, steps), do: steps

  def do_calc(number, step) when is_even(number) do
    do_calc(div(number, 2), step + 1)
  end

  def do_calc(number, step) do
    do_calc(3 * number + 1, step + 1)
  end
end
