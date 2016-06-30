module SumOfMultiples exposing (..)

import List exposing (any, filter, sum)

{- Return the sum of all the multiples of the `Int`s in `xs`, up to,
but not including, a given `Int`.
-}
sumOfMultiples : List Int -> Int -> Int
sumOfMultiples xs = sum << anyDivides xs << fromOneToExclusive

{- Given lists of numbers, `xs` and `ys`, determine the list of
every `y` in `ys` such that some `x` in `xs` that `divides` `y`.
-}
anyDivides : List Int -> List Int -> List Int
anyDivides = filter << flip (any << flip divides)

-- Return a list of `number`s from `1` to the given one, exclusive.
fromOneToExclusive : number -> List number
fromOneToExclusive n = [1..pred n]

-- Determine whether an `Int` divides another.
divides : Int -> Int -> Bool
divides d n = n `rem` d == 0

-- Return the predecessor of a `number`, i.e. subtract one from it.
pred : number -> number
pred n = n - 1
