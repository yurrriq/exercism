defmodule Hamming do
  @doc """
  Returns number of differences between two strands of DNA, known as the Hamming Distance.

  ## Examples

  iex> Hamming.hamming_distance('AAGTCATA', 'TAGCGATC')
  {:ok, 4}
  """
  @spec hamming_distance([char], [char]) :: {:ok, non_neg_integer} | {:error, String.t()}
  def hamming_distance(strand1, strand2) do
    hamming_distance(0, strand1, strand2)
  end

  defp hamming_distance(sum, [], []) do
    {:ok, sum}
  end

  defp hamming_distance(_, [], _) do
    {:error, "strands must be of equal length"}
  end

  defp hamming_distance(_, _, []) do
    {:error, "strands must be of equal length"}
  end

  defp hamming_distance(sum, [ha | ta], [hb | tb]) when ha == hb do
    hamming_distance(sum, ta, tb)
  end

  defp hamming_distance(sum, [_ | ta], [_ | tb]) do
    hamming_distance(sum + 1, ta, tb)
  end
end
