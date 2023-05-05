-- |
-- Module      : LeapYear
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Determining if a year is a leap year.
module LeapYear (isLeapYear) where

-- | Given a year, returns @True@ if it is a leap year, otherwise @False@.
isLeapYear :: Integral a => a -> Bool
isLeapYear year
  | 400 `divides` year = True
  | 100 `divides` year = False
  | 4 `divides` year = True
  | otherwise = False

-- | Returns @True@ if @ d | n@, otherwise @False@.
divides :: Integral a => a -> a -> Bool
d `divides` n = n `rem` d == 0
