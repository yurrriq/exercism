{-|
Module      : Gigasecond
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Calculating when someone turned or will turn 1 Gs old.
-}

module Gigasecond (fromDay) where

import           Data.Time.Clock (UTCTime, addUTCTime)

-- | Given a time, returns the time a gigasecond later.
fromDay :: UTCTime -> UTCTime
fromDay = addUTCTime 1000000000
