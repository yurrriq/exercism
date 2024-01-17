defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t()) :: map()
  def count(sentence), do: words(sentence) |> Enum.reduce(%{}, &inc_counter/2)

  @non_separator ~r/[^\p{L}\d-]+/u

  defp words(sentence), do: Regex.split(@non_separator, sentence, trim: true)

  defp inc_counter(key, dict) do
    Dict.update(dict, String.downcase(key), 1, &inc/1)
  end

  defp inc(x), do: x + 1
end
