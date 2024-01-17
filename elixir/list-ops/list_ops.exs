defmodule ListOps do
  # Please don't use any external modules (especially List) in your
  # implementation. The point of this exercise is to create these basic functions
  # yourself.
  #
  # Note that `++` is a function from an external module (Kernel, which is
  # automatically imported) and so shouldn't be used either.

  @spec count(list) :: non_neg_integer
  def count(l), do: reduce(l, 0, fn _, sum -> sum + 1 end)

  @spec reverse(list) :: list
  def reverse(l), do: reverse(l, [])

  defp reverse([], acc), do: acc
  defp reverse([h | t], acc), do: reverse(t, [h | acc])

  @spec map(list, (any -> any)) :: list
  def map(l, f), do: reverse(reduce(l, [], &[f.(&1) | &2]))

  @spec filter(list, (any -> as_boolean(term))) :: list
  def filter(l, f) do
    reverse(l)
    |> reduce(
      [],
      &if f.(&1) do
        [&1 | &2]
      else
        &2
      end
    )
  end

  @type acc :: any
  @spec reduce(list, acc, (any, acc -> acc)) :: acc
  def reduce([], acc, _f), do: acc
  def reduce([h | t], acc, f), do: reduce(t, f.(h, acc), f)

  @spec append(list, list) :: list
  def append(xs, ys), do: reverse(xs) |> reduce(ys, fn x, acc -> [x | acc] end)

  @spec concat([[any]]) :: [any]
  def concat(ll), do: reverse(ll) |> reduce([], &append/2)
end
