defmodule Prompt do
  @moduledoc """
  String predicates for `Teenager`.
  """

  @doc "Returns true if `input` ends with \"?\", otherwise false."
  def question?(input) do
    String.last(input) == "?"
  end

  @doc "Returns true if `input` is empty or only whitespace, otherwise false."
  def silent?(input) do
    String.strip(input) == ""
  end

  @doc """
  Returns true if `input` contains at least one uppercase letter and all
  letters are uppercase, otherwise false.
  """
  def yelled?(input) do
    some_upper?(input) && String.upcase(input) == input
  end

  defp some_upper?(input) do
    String.match?(input, ~r/[A-Z]/)
  end
end

defmodule Teenager do
  @moduledoc """
  Responds as a teenager would.

  ## Examples

      iex> Teenager.hey("Teenagers are lackadaisical.")
      "Whatever."

      iex> Teenager.hey("Are you lackadaisical?")
      "Sure."

      iex> Teenager.hey("STOP BEING LACKADAISICAL!.")
      "Whoa, chill out!"

      iex> Teenager.hey("   ")
      "Fine. Be that way!"
  """

  @doc "Given some input, returns a lackadaisical response."
  def hey(input) do
    import Prompt
    cond do
      question?(input) -> "Sure."
      silent?(input)   -> "Fine. Be that way!"
      yelled?(input)   -> "Whoa, chill out!"
      true             -> "Whatever."
    end
  end
end
