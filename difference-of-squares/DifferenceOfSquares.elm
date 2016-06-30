module DifferenceOfSquares exposing (squareOfSum, sumOfSquares, difference)

{- Compute the square of the sum of the first `n` natural numbers.

    squareOfSum  5 ==  225
    squareOfSum 10 == 3025
-}
squareOfSum : Int -> Int
squareOfSum n = square (n * succ n // 2)
-- \left(\sum\limits_{k=1}^n k\right)^2 = \left(\frac{n(n + 1)}{2}\right)^2

{- Compute the sum of the squares of the first `n` natural numbers.

    sumOfSquares  5 ==  55
    sumOfSquares 10 == 385
-}
sumOfSquares : Int -> Int
sumOfSquares n = n * succ n * succ (2 * n) // 6
-- \sum\limits_{k=1}^n k^2 = \frac{n(n + 1)(2n + 1)}{6}

{- Compute the difference between the sum of the squares
and the square of the sum of the first `n` natural numbers.

    difference  5 ==  170
    difference 10 == 2640
-}
difference : Int -> Int
difference n = squareOfSum n - sumOfSquares n
-- \left(\sum\limits_{k=1}^n k\right)^2 - \sum\limits_{k=1}^n k^2

-- Compute the square of a number.
square : number -> number
square = flip (^) 2

-- Return the successor of a number, i.e. add one to it.
succ : number -> number
succ n = n + 1
