{-|
Module      : Meetup
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Calculating the date of meetups.
-}
module Meetup (Weekday(..), Schedule(..), meetupDay) where

import Data.Function (on)
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

-- | Days of the week enumerator.
data Weekday = Someday
             | Monday   | Tuesday | Wednesday | Thursday | Friday
             | Saturday | Sunday
             deriving (Enum, Eq, Show)

-- | Enumerator for specifying an occurrence of a 'Weekday' in a month.
data Schedule = First | Second | Third | Fourth | Teenth | Last 
              deriving (Enum, Eq)

-- | A year is an 'Integer'.
type Year = Integer

-- | A month is an 'Int'.
type Month = Int

-- | A week date (as in "Data.Time.Calendar.WeekDate") is a tuple of 'Year',
-- 'Month' and 'Int'.
type WeekDate = (Year, Month, Int)

-- | Given a 'Year' @y@ and 'Month' @m@, returns the 'WeekDate' representing
-- the first day of @m@ in @y.@
firstWeekDate :: Year -> Month -> WeekDate
firstWeekDate = toWeekDate .: (flip flip 1 . fromGregorian)

-- | Given a predicate @p@ and a list @xs@, returns the first @x@ in @xs@ where
-- @p x@ evaluates to @True@.
--
-- __Note__: This is unsafe. @filter xs@ must return a non-empty list. 
findFirst :: (a -> Bool) -> [a] -> a
findFirst = head .: filter

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)

-- | Given a 'Year' @y@, 'Month' @m@ and 'Weekday' @wd@, returns the list of
-- days of the month for each occurrence of @wd@ in @m@ in @y@.
dates :: Year -> Month -> Weekday -> [Int]
dates year month weekday =
  takeWhile (<= numDays) [startDay, startDay+7..]
    where numDays  = gregorianMonthLength year month
          startDay = findWeekday year month weekday

-- | Given a 'WeekDate', returns its day of the week, which is most useful in
-- creating 'Weekday's.
dayOfWeek :: WeekDate -> Int
dayOfWeek (_, _, x) = x

-- | Given two 'Weekday's, returns their absolute difference, in number of days.
diffWeekday :: Weekday -> Weekday -> Int
diffWeekday = ((`mod` 7) . (7 +)) .: (-) `on` fromEnum

-- | Given a 'Year' @y@, 'Month' @m@ and 'Weekday' @wd@, returns the
-- day of the month of the first occurrence of @wd@ in @m@ in @y@.
findWeekday :: Year -> Month -> Weekday -> Int
findWeekday = (succ .: flip diffWeekday) .: firstWeekday

-- | Given a 'Year' @y@ and 'Month' @m@, returns the first 'Weekday'
-- of @m@ in @y@.
firstWeekday :: Year -> Month -> Weekday
firstWeekday = (toEnum . dayOfWeek) .: firstWeekDate

-- | Given a 'Schedule', 'Weekday', 'Year' and 'Month', returns the specified
-- 'Data.Time.Calendar.Day'.
meetupDay :: Schedule -> Weekday -> Year -> Month -> Day
meetupDay schedule weekday year month = fromGregorian year month day
  where dates' = dates year month weekday
        day    = case schedule of
                   Last   -> last dates'
                   Teenth -> findFirst (> 12) dates'
                   nth    -> dates' !! fromEnum nth
