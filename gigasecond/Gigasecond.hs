{-|
Module      : Gigasecond
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Calculating when someone turned or will turn 1 Gs old.
-}

module Gigasecond (
  fromDay
  -- * Usage
  -- $usage
  ) where

import Data.Time.Clock (UTCTime, addUTCTime)

-- | Given a time, returns the time a gigasecond later.
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime 1000000000

{- $usage
>>> import Data.Time.Format (readTime)
>>> import System.Locale (defaultTimeLocale, iso8601DateFormat)
>>> let fromString = readTime defaultTimeLocale (iso8601DateFormat (Just "%T%Z")) :: String -> UTCTime
2020-10-18 04:21:40 UTC
-}
