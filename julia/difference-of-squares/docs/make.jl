using Documenter, DifferenceOfSquares
using Documenter.Remotes: GitHub

makedocs(
  sitename="Difference of Squares",
  remotes=nothing,
  # repo=GitHub("yurrriq", "exercism"),
  modules=[DifferenceOfSquares],
)
