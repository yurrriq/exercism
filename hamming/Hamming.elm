module Hamming exposing (distance)

{-| This module calculates the Hamming difference between two DNA strands.

@docs distance

-}

import String exposing (isEmpty, uncons)
import Maybe  exposing (andThen)

{-| Calculate the Hamming difference between two DNA strands. -}
distance : String -> String -> Maybe Int
distance = maybeZipFoldlString ((+) <<: neToInt) 0

{-| From Haskell's `Data.Function.Pointless`.

    (f <<: g) x y = f (g x y)

or,

    f <<: g = curry (f << uncurry g)
-}
(<<:) : (c -> d) -> (a -> b -> c) -> a -> b -> d
(<<:) = (<<) << (<<)

{-| Convert a `Bool` to an `Int`.

    boolToInt True  = 1
    boolToInt False = 0
-}
boolToInt : Bool -> Int
boolToInt b =
  case b of
    True  -> 1
    False -> 0

-- zipFoldl specialized for `String`. Return `Nothing` on unequal lengths.
maybeZipFoldlString : (Char -> Char -> a -> a)
                    -> a
                    -> String
                    -> String
                    -> Maybe a
maybeZipFoldlString f acc a b =
  -- If both strings are empty, we're done...
  if   isEmpty a && isEmpty b
  -- ... and can return `Just` the result.
  then Just acc
  -- Otherwise, attempt to uncons the strings and recur.
  else uncons a `andThen`
         (\(ha,ta) ->
           uncons b `andThen`
             (\(hb,tb) ->
               maybeZipFoldlString f (f ha hb acc) ta tb))

-- Call `boolToInt` on the result of `/=`.
neToInt : a -> a -> Int
neToInt = boolToInt <<: (/=)
