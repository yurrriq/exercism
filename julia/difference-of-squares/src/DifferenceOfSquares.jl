module DifferenceOfSquares

export square_of_sum, sum_of_squares, difference

@doc raw"""
    square_of_sum(n::Integer)::Integer

Square the sum of the first `n` positive integers.

```math
\begin{align*}
  \left(\sum_{x=1}^n x\right)^2
  &= \left(\frac{n(n + 1)}{2}\right)^2 \\
  &= \frac{n^2 + 2n^3 + n^4}{4}
\end{align*}
```
"""
square_of_sum(n::Integer)::Integer = evalpoly(n, (0, 0, 1, 2, 1)) ÷ 4

@doc raw"""
    sum_of_squares(n::Integer)::Integer

Sum the squares of the first `n` positive integers.

```math
\begin{align*}
  \sum_{x=1}^n x^2
  &= \frac{n(n + 1)(2n + 1)}{6} \\
  &= \frac{n + 3n^2 + 2n^3}{6}
\end{align*}
```
"""
sum_of_squares(n::Integer)::Integer = evalpoly(n, (0, 1, 3, 2)) ÷ 6

@doc raw"""
    difference(n::Integer)::Integer

Subtract the sum of squares from square of the sum of the first `n` positive integers.

```math
\begin{align*}
  \left(\sum_{x=1}^n x\right)^2 - \sum_{x=1}^n x^2
  &= \left(\frac{n(n + 1)}{2}\right)^2 - \frac{n(n + 1)(2n + 1)}{6} \\
  &= \frac{n^2 + 2n^3 + n^4}{4} - \frac{n + 3n^2 + 2n^3}{6} \\
  &= \frac{-2n - 3n^2 + 2n^3 + 3n^4}{12}
\end{align*}
```
"""
difference(n::Integer)::Integer = evalpoly(n, (0, -2, -3, 2, 3)) ÷ 12

end # module DifferenceOfSquares
