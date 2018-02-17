{-|
Module      : Meetup
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Calculating the date of meetups.
-}

module Meetup (Weekday(..), Schedule(..), Month, WeekDate, Year, meetupDay) where

import           Data.Function               (on)
import           Data.Time.Calendar          (Day, fromGregorian,
                                              gregorianMonthLength)
import           Data.Time.Calendar.WeekDate (toWeekDate)

-- | Days of the week enumerator.
data Weekday = Someday
             | Monday   | Tuesday | Wednesday | Thursday | Friday
             | Saturday | Sunday
             deriving (Enum, Eq, Show)

-- | Enumerator for specifying an occurrence of a 'Weekday' in a month.
data Schedule = First | Second | Third | Fourth | Teenth | Last
              deriving (Enum, Eq, Show)

-- | A year is an 'Integer'.
type Year = Integer

-- | A month is an 'Int'.
type Month = Int

-- | A week date (as in "Data.Time.Calendar.WeekDate") is a tuple of 'Year',
-- 'Month' and 'Int'.
type WeekDate = (Year, Month, Int)

-- | Given a 'Schedule', 'Weekday', 'Year' and 'Month', returns the specified
-- 'Data.Time.Calendar.Day'.
meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday year month = fromGregorian year month day
  where
    day = case schedule of
      Last   ->
        let lastWeekday = toWeekday $ fromGregorian year month monthLength
            monthLength = gregorianMonthLength year month
            toWeekday   = (toEnum . dayOfWeek . toWeekDate)
        in  monthLength - diffWeekday lastWeekday weekday
      Teenth -> 13 + succ date `mod` 7
      nth    -> date + (7 * fromEnum nth)
      where date = firstDate year month weekday

-- | Given a 'Year' @y@, 'Month' @m@ and 'Weekday' @wd@, returns the
-- day of the month of the first occurrence of @wd@ in @m@ in @y@.
firstDate :: Year -> Month -> Weekday -> Int
firstDate = (succ .: flip diffWeekday) .: firstWeekday

-- | Given a 'Year' @y@ and 'Month' @m@, returns the first 'Weekday'
-- of @m@ in @y@.
firstWeekday :: Year -> Month -> Weekday
firstWeekday = (toEnum . dayOfWeek) .: firstWeekDate

-- | Given two 'Weekday's, returns their absolute difference, in number of days.
diffWeekday :: Weekday -> Weekday -> Int
diffWeekday = ((`mod` 7) . (7 +)) .: (-) `on` fromEnum

-- | Given a 'Year' @y@ and 'Month' @m@, returns the 'WeekDate' representing
-- the first day of @m@ in @y.@
firstWeekDate :: Year -> Month -> WeekDate
firstWeekDate = toWeekDate .: (flip flip 1 . fromGregorian)

-- | Given a 'WeekDate', returns its day of the week, which is most useful in
-- creating 'Weekday's.
dayOfWeek :: WeekDate -> Int
dayOfWeek (_, _, x) = x

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
