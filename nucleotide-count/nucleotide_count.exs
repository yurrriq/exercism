defmodule NucleotideCount do
  @nucleotides [?A, ?C, ?G, ?T]


  @doc """
  Counts individual nucleotides in a NucleotideCount strand.

  ## Examples

  iex> NucleotideCount.count('AATAA', ?A)
  4

  iex> NucleotideCount.count('AATAA', ?T)
  1
  """
  @spec count([char], char) :: non_neg_integer
  def count(strand, nucleotide) do
    List.foldl(strand, 0, &do_count(nucleotide, &1, &2))
  end

  @spec do_count(char, char, non_neg_integer) :: non_neg_integer
  defp do_count(nucleotide, nucleotide, count), do: count + 1
  defp do_count(_, _, count), do: count


  @doc """
  Returns a summary of counts by nucleotide.

  ## Examples

  iex> NucleotideCount.histogram('AATAA')
  %{?A => 4, ?T => 1, ?C => 0, ?G => 0}
  """
  @spec histogram([char]) :: map
  def histogram(strand) do
    List.foldl(strand, Map.new(@nucleotides, &({&1, 0})), &do_histogram/2)
  end


  @spec do_histogram(char, map) :: map
  defp do_histogram(nucleotide, acc) do
    Map.update!(acc, nucleotide, &(&1 + 1))
  end
end
