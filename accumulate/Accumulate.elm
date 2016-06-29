module Accumulate exposing (accumulate)

import List exposing (foldr)

{-| Apply a function to every element of a list.

    accumulate square         [1,2,3]           == [1,4,9]
    accumulate String.toUpper ["hello","world"] == ["HELLO","WORLD"]
-}
accumulate : (a -> b) -> List a -> List b
accumulate f xs = foldr ((::) << f) [] xs

{- Alternate version that (ab)uses Maybe.

import List  exposing (head, tail)
import Maybe exposing (map, withDefault)

accumulate f xs =
  let g x = f x :: accumulate f (withDefault [] (tail xs))
  in  withDefault [] (map g (head xs))
-}

{- Alternate version that pattern matches on xs.

accumulate f xs =
  case xs of
    hd::tl -> f hd :: accumulate f tl
    []     -> []
-}
