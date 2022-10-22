module Yacht
  ( yacht,
    Category (..),
  )
where

import Data.List (group, sort)

data Category
  = Ones
  | Twos
  | Threes
  | Fours
  | Fives
  | Sixes
  | FullHouse
  | FourOfAKind
  | LittleStraight
  | BigStraight
  | Choice
  | Yacht

yacht :: Category -> [Int] -> Int
yacht Ones dice = sum (filter (== 1) dice)
yacht Twos dice = sum (filter (== 2) dice)
yacht Threes dice = sum (filter (== 3) dice)
yacht Fours dice = sum (filter (== 4) dice)
yacht Fives dice = sum (filter (== 5) dice)
yacht Sixes dice = sum (filter (== 6) dice)
yacht FullHouse dice =
  case group (sort dice) of
    [[_, _], [_, _, _]] -> sum dice
    [[_, _, _], [_, _]] -> sum dice
    _ -> 0
yacht FourOfAKind dice =
  case group (sort dice) of
    [[x, _, _, _], [_]] -> 4 * x
    [[_], [x, _, _, _]] -> 4 * x
    [[x, _, _, _, _]] -> 4 * x
    _ -> 0
yacht LittleStraight dice
  | sort dice == [1, 2, 3, 4, 5] = 30
yacht BigStraight dice
  | sort dice == [2, 3, 4, 5, 6] = 30
yacht Choice dice = sum dice
yacht Yacht (die : dice)
  | all (== die) dice = 50
yacht _category _dice = 0
