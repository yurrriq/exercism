module Leap exposing (isLeapYear)

divides : Int -> Int -> Bool
divides d n = n `rem` d == 0

isLeapYear : Int -> Bool
isLeapYear year =
  400 `divides` year ||
    (not (100 `divides` year) && 4 `divides` year)
