-- |
-- Module      : Clock
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Handling time without dates.
module Clock where

import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.List.Split (splitOn)
import Text.Printf (printf)

-- | A number of hours.
type Hour = Integer

-- | A number of minutes.
type Min = Integer

-- | A 'Clock' represents a dateless time.
newtype Clock = Clock
  { -- | The number of minutes, floored to the 'day'.
    minutes :: Integer
  }
  deriving (Eq, Show)

-- | Specialized 'Clock' arithmetic, operating on 'minutes'.
instance Num Clock where
  fromInteger = Clock . fromInteger . (`mod` day)

  (+) = fromInteger .: (+) `on` minutes
  (*) = fromInteger .: (*) `on` minutes
  (-) = fromInteger .: (-) `on` minutes

  abs = id

  signum = Clock . signum . minutes

  negate = (-) (Clock 0)

-- | Given a number of hours and a number of minutes, return a 'Clock'
-- representing that time, floored to the day, e.g.
-- @toString (fromHourMin 24 42)@ yields @"00:42"@.
fromHourMin :: Hour -> Min -> Clock
fromHourMin = fromIntegral .: (+) . hourToMin

-- | Given a string of the form, @hh:mm@, return a 'Clock' such that calling
-- 'toString' on the 'Clock' returns the given string.
fromString :: String -> Clock
fromString = go . splitOn ":"
  where
    go [h, m] = (fromHourMin `on` read) h m
    go _ = error "invalid string"

-- | Given a 'Clock', return a string representation of the form, @hh:mm@.
toString :: Clock -> String
toString = uncurry (printf "%02d:%02d") . (`divMod` 60) . minutes

-- | The number of minutes in a day, @1440@.
day :: Min
day = hourToMin 24

-- | Given a number of hours, return the equivalent number of minutes.
hourToMin :: Hour -> Min
hourToMin = (60 *)
