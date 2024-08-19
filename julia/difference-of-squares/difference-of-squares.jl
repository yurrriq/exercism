"Square the sum of the first `n` positive integers"
square_of_sum(n::Integer)::Integer = ((n * (n + 1)) ÷ 2)^2

"Sum the squares of the first `n` positive integers"
sum_of_squares(n::Integer)::Integer = (n * (n + 1) * (2 * n + 1)) ÷ 6

"Subtract the sum of squares from square of the sum of the first `n` positive ints"
difference(n::Integer)::Integer = square_of_sum(n) - sum_of_squares(n)
