{-|
Module      : Sublist
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Determining if a list is a sublist of another list.
-}
module Sublist (
  Sublist(..)
  , sublist
  ) where

import Data.List (isInfixOf)

{-|
Result of comparing two lists to determine if the first is
a sublist of the second.
-}
data Sublist
  -- | The two lists are equal.
  = Equal
  -- | The first list is a fully contained, in order, in the second list.
  | Sublist
  -- | The first fully contains, in order, the second list.
  | Superlist
  -- | Neither list fully contains the other.
  | Unequal
  deriving (Show, Eq)

{-|
Given two lists, returns a 'Sublist' based on
the first list compared to the second.
-}
sublist :: Eq a => [a] -> [a] -> Sublist
sublist a b
  | a == b           = Equal
  | a `isInfixOf` b = Sublist
  | b `isInfixOf` a = Superlist
  | otherwise       = Unequal
