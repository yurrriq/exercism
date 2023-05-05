-- |
-- Module      : Hexadecimal
-- Copyright   : Public Domain
-- License     : Unlicense
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Converting hexadecimal numbers, represented as strings,
-- to their decimal equivalents.
module Hexadecimal (hexToInt) where

import Control.Monad (foldM)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (find)
import Data.Maybe (fromMaybe)

-- | TODO: Write docstring
hexToInt :: String -> Int
hexToInt = fromMaybe 0 . foldM ((. hexToIntM) . (<$+>) . (16 *)) 0

-- | TODO: Write docstring
hexToIntM :: Char -> Maybe Int
hexToIntM c =
  (`snd` c)
    <$> find
      (`fst` c)
      [ (inRange ('0', '9'), distanceFrom '0'),
        (inRange ('a', 'f'), (+ 10) . distanceFrom 'a'),
        (inRange ('A', 'F'), (+ 10) . distanceFrom 'A')
      ]
  where
    distanceFrom :: Char -> Char -> Int
    distanceFrom = flip (-) `on` fromEnum

-- | Given an 'Int' @x@ and a 'Maybe' @Int y@, if @y@ is a @Just@,
-- returns @Just (x + y)@, otherwise @Nothing@.
(<$+>) :: Int -> Maybe Int -> Maybe Int
(<$+>) = (<$>) . (+)
