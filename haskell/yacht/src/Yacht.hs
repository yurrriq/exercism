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
yacht Ones dice = pips 1 dice
yacht Twos dice = pips 2 dice
yacht Threes dice = pips 3 dice
yacht Fours dice = pips 4 dice
yacht Fives dice = pips 5 dice
yacht Sixes dice = pips 6 dice
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
  | sort dice == [1 .. 5] = 30
yacht BigStraight dice
  | sort dice == [2 .. 6] = 30
yacht Choice dice = sum dice
yacht Yacht (die : dice)
  | all (== die) dice = 50
yacht _category _dice = 0

pips :: Int -> [Int] -> Int
pips n = sum . filter (== n)
