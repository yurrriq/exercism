{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : TwelveDays
-- Description : Output the lyrics to "The Twelve Days of Christmas".
-- Copyright   : (c) Eric Bailey, 2022
-- License     : MIT
-- Maintainer  : eric@ericb.me
-- Stability   : experimental
-- Portability : portable
module TwelveDays where

import Data.Finite (Finite, finite)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.TypeNats (KnownNat)

-- | Recite the lyrics to "The Twelve Days of Christmas" for a given 'Int' range of days.
recite :: Int -> Int -> [Text]
recite start stop = reciteFinite (finiteInt start) (finiteInt stop)

-- | In "The Twelve Days of Christmas", there are strictly less than 13 days.
type Day = Finite 13

-- | Recite the lyrics to "The Twelve Days of Christmas" for a given range of 'Day's.
reciteFinite :: Day -> Day -> [Text]
reciteFinite start stop = [line n | n <- [start .. stop]]

-- | The line for a given 'Day'.
line :: Day -> Text
line n = T.unwords (startLine n <> finishLine n)

-- | "On the /nth/ day of Christmas..."
startLine :: Day -> [Text]
startLine n = ["On the", ordinal n, "day of Christmas my true love gave to me:"]

-- | Recite the remaining 'gift's.
finishLine :: Day -> [Text]
finishLine 1 = [countGift 1 <> "."]
finishLine n =
  [countGift m <> "," | m <- [n, n - 1 .. 2]]
    <> ["and"]
    <> finishLine 1

-- | The 'gift' on a given 'Day', starting with how many.
countGift :: Day -> Text
countGift n = numeral n <> " " <> gift n

-- | The gift on a given 'Day'.
gift :: Day -> Text
gift 1 = "Partridge in a Pear Tree"
gift 2 = "Turtle Doves"
gift 3 = "French Hens"
gift 4 = "Calling Birds"
gift 5 = "Gold Rings"
gift 6 = "Geese-a-Laying"
gift 7 = "Swans-a-Swimming"
gift 8 = "Maids-a-Milking"
gift 9 = "Ladies Dancing"
gift 10 = "Lords-a-Leaping"
gift 11 = "Pipers Piping"
gift 12 = "Drummers Drumming"
gift _ = undefined

-- | Convert a 'Day' to its number word.
--
-- > numeral 1 = "a"
numeral :: Day -> Text
numeral 1 = "a"
numeral 2 = "two"
numeral 3 = "three"
numeral 4 = "four"
numeral 5 = "five"
numeral 6 = "six"
numeral 7 = "seven"
numeral 8 = "eight"
numeral 9 = "nine"
numeral 10 = "ten"
numeral 11 = "eleven"
numeral 12 = "twelve"
numeral _ = undefined

-- | Convert a 'Day' to its ordinal number word.
ordinal :: Day -> Text
ordinal 1 = "first"
ordinal 2 = "second"
ordinal 3 = "third"
-- ordinal 4 = "fourth"
ordinal 5 = "fifth"
-- ordinal 6 = "sixth"
-- ordinal 7 = "seventh"
ordinal 8 = "eighth"
ordinal 9 = "ninth"
-- ordinal 10 = "ten"
-- ordinal 11 = "eleven"
ordinal 12 = "twelfth"
ordinal n = numeral n <> "th"

-- | Convert an 'Int' into a 'Finite', throwing an error if the input is out of bounds.
finiteInt :: KnownNat n => Int -> Finite n
finiteInt n = finite (fromIntegral n)
