defmodule Raindrops do
  @doc """
  Returns a string based on raindrop factors.

  - If the number contains 3 as a prime factor, output 'Pling'.
  - If the number contains 5 as a prime factor, output 'Plang'.
  - If the number contains 7 as a prime factor, output 'Plong'.
  - If the number does not contain 3, 5, or 7 as a prime factor,
    just pass the number's digits straight through.
  """

  @raindrops %{
    7 => "Plong",
    5 => "Plang",
    3 => "Pling"
  }

  defguardp divides(divisor, number) when rem(number, divisor) == 0

  @spec convert(pos_integer) :: String.t()
  def convert(number) do
    @raindrops
    |> Enum.flat_map(&do_convert(&1, number))
    |> format_sounds(number)
  end

  defp do_convert({divisor, sound}, number) when divides(divisor, number), do: [sound]
  defp do_convert(_, _), do: []

  defp format_sounds([], number), do: Integer.to_string(number)
  defp format_sounds(sounds, _), do: Enum.join(sounds)
end
