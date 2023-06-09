defmodule ProteinTranslation do
  @amino_acids %{
                 ~w(AUG) => "Methionine",
                 ~w(UUU UUC) => "Phenylalanine",
                 ~w(UUA UUG) => "Leucine",
                 ~w(UCU UCC UCA UCG) => "Serine",
                 ~w(UAU UAC) => "Tyrosine",
                 ~w(UGU UGC) => "Cysteine",
                 ~w(UGG) => "Tryptophan",
                 ~w(UAA UAG UGA) => "STOP"
               }
               |> Enum.reduce(%{}, fn {codons, protein}, outer ->
                 Enum.reduce(codons, outer, &Map.put(&2, &1, protein))
               end)

  @doc """
  Given an RNA string, return a list of proteins specified by codons, in order.
  """
  @spec of_rna(String.t()) :: {:ok, list(String.t())} | {:error, String.t()}
  def of_rna(rna), do: of_rna(rna, [])

  defp of_rna(<<x, y, z, tail::binary>>, acc) do
    case of_codon(<<x, y, z>>) do
      {:ok, "STOP"} -> of_rna("", acc)
      {:ok, protein} -> of_rna(tail, [protein | acc])
      {:error, "invalid codon"} -> {:error, "invalid RNA"}
    end
  end

  defp of_rna("", acc), do: {:ok, Enum.reverse(acc)}
  defp of_rna(_, _), do: {:error, "invalid RNA"}

  @doc """
  Given a codon, return the corresponding protein

  UGU -> Cysteine
  UGC -> Cysteine
  UUA -> Leucine
  UUG -> Leucine
  AUG -> Methionine
  UUU -> Phenylalanine
  UUC -> Phenylalanine
  UCU -> Serine
  UCC -> Serine
  UCA -> Serine
  UCG -> Serine
  UGG -> Tryptophan
  UAU -> Tyrosine
  UAC -> Tyrosine
  UAA -> STOP
  UAG -> STOP
  UGA -> STOP
  """
  @spec of_codon(String.t()) :: {:ok, String.t()} | {:error, String.t()}
  def of_codon(codon) do
    case Map.get(@amino_acids, codon) do
      "STOP" -> {:ok, "STOP"}
      nil -> {:error, "invalid codon"}
      protein -> {:ok, protein}
    end
  end
end
