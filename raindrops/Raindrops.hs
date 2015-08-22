{-|
Module      : Raindrops
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Converting numbers to raindrops, based on their prime factors.
-}

module Raindrops (convert) where

import           Control.Monad (ap, join)
import           Data.Bool     (bool)
import           Data.Maybe    (fromMaybe, mapMaybe)

-- | Given a number @n@, convert it to a string, the contents of which
-- depends on @n@'s prime factors.
--
--  - If 3 divides @n@, output "Pling".
--  - If 5 divides @n@, output "Plang".
--  - If 7 divides @n@, output "Plong".
--  - If neither 3 nor 5 nor 7 divides @n@, return the @n@ as a string."
convert :: Int -> String
convert x = fromIff (show x) (not . null) (join (mapMaybe (go x) raindrops))
  where
    raindrops = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    go = uncurry . iffThen . flip divides
    d `divides` n = n `rem` d == 0

-- | Given a predicate @p@ and a value @x@, return 'Just' @x@ if @p x@ holds,
-- otherwise 'Nothing'.
iff :: (a -> Bool) -> a -> Maybe a
iff = ap (bool Nothing . Just)

-- | Given a predicate @p@, a value to test @x@ and a desired value @y@,
-- return 'Just' @y@ if @p x@ holds, otherwise 'Nothing'.
iffThen :: (a -> Bool) -> a -> b -> Maybe b
iffThen p x = (iff p x >>) . Just
-- iffThen = (((. Just) . (>>)) .) . iff
-- iffThen p x = (iff p x >>) . Just

-- | Given a default value @d@, a predicate @p@ and a value to test @x@,
-- | return @x@ if @p x@ holds, otherwise @d@.
fromIff :: a -> (a -> Bool) -> a -> a
fromIff d p = fromMaybe d . iff p
