defmodule Words do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  @spec count(String.t) :: map()
  def count(sentence), do: Enum.reduce(words(sentence), %{}, &update_counter/2)

  defp update_counter(word, counter) do
    Dict.update(counter, String.downcase(word), 1, &inc/1)
  end

  @non_separator ~r/[^\p{L}\d-]+/u

  defp words(sentence) do
    Enum.filter(Regex.split(@non_separator, sentence), &not_empty?/1)
  end

  defp not_empty?(""), do: false
  defp not_empty?(_),  do: true

  defp inc(x), do: x + 1
end
