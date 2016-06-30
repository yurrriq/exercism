module Strain exposing (keep, discard)

import List exposing (foldr)

{- Keep only values that satisfy the predicate.

    keep isEven [1..6] == [2,4,6]
-}
keep : (a -> Bool) -> List a -> List a
keep f = foldr (\x acc -> bool acc (x :: acc) (f x)) []

{- Disard values that satisfy the predicate.

    keep isEven [1..6] == [1,3,5]
-}
discard : (a -> Bool) -> List a -> List a
discard f = keep (not << f)

-- Equivalent to if p then y else x.
bool : a -> a -> Bool -> a
bool x y p = if p then y else x
