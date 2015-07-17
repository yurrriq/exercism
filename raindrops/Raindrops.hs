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

convert :: Int -> String
convert x = fromIff (show x) (not . null) (join (mapMaybe (go x) raindrops))
  -- ap (`bool` show x) null $ join (mapMaybe (go x) raindrops)
  -- fromMaybe (show x) $ iff (not . null) (join (mapMaybe (go x) raindrops))
  where
    raindrops = [(3, "Pling"), (5, "Plang"), (7, "Plong")]
    go = uncurry . iffThen . flip divides
    d `divides` n = n `rem` d == 0

iff :: (a -> Bool) -> a -> Maybe a
iff = ap (bool Nothing . Just)

iffThen :: (a -> Bool) -> a -> b -> Maybe b
iffThen p x d = iff p x >> Just d
-- iffThen = (((. Just) . (>>)) .) . iff
-- iffThen p x = (iff p x >>) . Just

fromIff :: a -> (a -> Bool) -> a -> a
fromIff d p = fromMaybe d . iff p
