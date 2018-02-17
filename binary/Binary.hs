{-# LANGUAGE LambdaCase #-}
{-|
Module      : Binary
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Converting string binary numbers to their decimal equivalents.
-}

module Binary (
  toDecimal,
  -- * Usage
  -- $usage
  charToBinaryM,
  (<$+>)
  ) where

import           Control.Monad (foldM)
import           Data.Maybe    (fromMaybe)

-- | Given a 'String' representing a binary number, calls 'charToBinaryM' on
-- each character. If any call returns @Nothing@, the fold returns @Nothing@,
-- otherwise folds the list of @Just@ binary digits from left to right,
-- by doubling the accumulator (starting at @0@) and adding the @Just@ value.
-- After the the fold, the resulting @Maybe Int@ is unwrapped,
-- with @0@ as the default.
toDecimal :: String -> Int
toDecimal = fromMaybe 0 . foldM ((. charToBinaryM) . (<$+>) . (2*)) 0

{- $usage
>>> toDecimal "11010"
acc' = (2*) acc <$+> charToBinaryM c -- (   seen,         rest)
-----------------------------------------------------------
   0 = 0                             -- (     "",      "11010")
   1 = (2 *  0) + 1                  -- (    "1",       "1010")
   3 = (2 *  1) + 1                  -- (   "11",        "010")
   6 = (2 *  3) + 0                  -- (  "110",         "10")
  13 = (2 *  6) + 1                  -- ( "1101",          "0")
  26 = (2 * 13) + 0                  -- ("11010",           "")

>>> toDecimal "101"
   0 = 0                             -- (     "",        "101")
   1 = (2 *  0) + 1                  -- (    "1",         "01")
   2 = (2 *  1) + 0                  -- (   "10",          "1")
   5 = (2 *  2) + 1                  -- (  "101",           "")

>>> toDecimal "0xDEADBEEF"
   0 = 0                             -- (     "", "0xDEADBEEF")
   0 = (2 *  0) + 0                  -- (    "0",  "xDEADBEEF")
   ...
   0 = fromMaybe 0 Nothing
-}

-- | Given a 'Char', convert @'0'@ to @Just 0@ and @'1'@ to @Just 1@,
-- otherwise return @Nothing@.
charToBinaryM :: Char -> Maybe Int
charToBinaryM = \case '0' -> Just 0; '1' -> Just 1; _ -> Nothing

-- | Given an 'Int' @x@ and a 'Maybe' @Int y@, if @y@ is a @Just@,
-- return @Just (x + y)@, otherwise @Nothing@.
(<$+>) :: Int -> Maybe Int -> Maybe Int
(<$+>) = (<$>) . (+)
