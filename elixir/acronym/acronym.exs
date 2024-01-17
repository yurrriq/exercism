defmodule Acronym do
  @doc """
  Generate an acronym from a string.
  "This is a string" => "TIAS"
  """
  @spec abbreviate(String.t()) :: String.t()
  def abbreviate(string) do
    tokenize(string) |> Enum.map(&ffirst_capital/1) |> Enum.join()
  end

  @doc """
  Given a singleton string list, return the first letter of the string,
  capitalized.
  """
  @spec ffirst_capital([String.t()]) :: String.t()
  def ffirst_capital([string]), do: String.first(string) |> String.upcase()

  @word_or_camel ~r/\p{Lu}+\p{Ll}*|\p{L}+/u

  @doc """
  Match letters starting with an uppercase one, or any letters,
  splitting between a pair of adjacent lower and uppercase letters.
  """
  @spec tokenize(String.t()) :: [[String.t()]]
  def tokenize(string), do: Regex.scan(@word_or_camel, string)
end
