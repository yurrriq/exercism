module LeapYear (isLeapYear) where

isLeapYear :: Integral a => a -> Bool
isLeapYear year
  | 400 `divides` year = True
  | 100 `divides` year = False
  | 4   `divides` year = True
  | otherwise          = False

divides :: Integral a => a -> a -> Bool
d `divides` n = n `rem` d == 0
