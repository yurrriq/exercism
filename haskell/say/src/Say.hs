{-# LANGUAGE OverloadedStrings #-}

module Say
  ( inEnglish,
  )
where

import Control.Monad (liftM2)
import Data.List.NonEmpty ((!!))

inEnglish :: Integer -> Maybe String
inEnglish n
  | n < 0 =
      Nothing
  | n < 1000 =
      Just (sayUnder1000 n)
  | n `rem` 1000 == 0 =
      ("one " <>)
        <$> knownPowers !!? fromInteger (integerLog1000 n)
  | otherwise =
      sayPowerOfThousand power n
        =<< knownPowers !!? pred (fromInteger power)
  where
    power = integerLog1000 n

knownPowers :: [String]
knownPowers =
  [ "thousand",
    "million",
    "billion",
    "trillion",
    "quadrillion",
    "quintillion",
    "sextillion",
    "septillion",
    "octillion",
    "nonillion",
    "decillion",
    "undecillion",
    "duodecillion",
    "tredecillion",
    "quattuordecillion",
    "quindecillion",
    "sexdecillion",
    "septendecillion",
    "octodecillion",
    "novemdecillion",
    "vigintillion"
  ]

sayPowerOfThousand :: Integer -> Integer -> String -> Maybe String
sayPowerOfThousand power n label
  | power < 0 || power > toInteger (length knownPowers) =
      error "Called sayPowerOfThousand with power outside [0, length knownPowers]"
  | n < 0 || n >= 1000 ^ (22 :: Integer) =
      error "Called sayPowerOfThousand with n outside [0, 1000 ^ 22)"
  | otherwise =
      sayQuotRem (1000 ^ power) sayQuot glue inEnglish n
  where
    sayQuot q = Just (glueWith " " (sayUnder1000 q) label)
    glue = liftM2 (glueWith " ")

sayUnder1000 :: Integer -> String
sayUnder1000 n
  | n < 0 || n >= 1000 =
      error "Called sayUnder1000 with n outside [0, 1000)"
  | n < 100 =
      sayUnder100 n
  | otherwise =
      sayQuotRem 100 ((<> " hundred") . sayUnder100) (glueWith " ") sayUnder100 n

sayUnder100 :: Integer -> String
sayUnder100 n
  | n < 0 || n >= 100 =
      error "Called sayUnder100 with n outside [0, 100)"
  | n < 20 =
      sayUnder20 n
  | otherwise =
      sayQuotRem 10 sayTens (glueWith "-") sayUnder10 n

sayUnder20 :: Integer -> String
sayUnder20 10 = "ten"
sayUnder20 11 = "eleven"
sayUnder20 12 = "twelve"
sayUnder20 13 = "thirteen"
sayUnder20 15 = "fifteen"
sayUnder20 18 = "eighteen"
sayUnder20 n
  | n < 0 || n >= 20 =
      error "Called sayUnder20 with n outside [0, 20)"
  | n < 10 =
      sayUnder10 n
  | otherwise = sayUnder10 (n - 10) <> "teen"

sayUnder10 :: Integer -> String
sayUnder10 n
  | n < 0 || n >= 10 =
      error "Called sayUnder10 with n outside [0, 10)"
  | otherwise =
      english !! fromInteger n
  where
    english = "zero" :| ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

sayTens :: Integer -> String
sayTens n
  | n < 2 || n >= 10 =
      error "Called sayTens with n outside [2, 10)"
  | otherwise =
      english !! (fromInteger n - 2)
  where
    english = "twenty" :| ["thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"]

sayQuotRem :: Integer -> (Integer -> a) -> (a -> a -> a) -> (Integer -> a) -> Integer -> a
sayQuotRem divisor sayQuot glue sayRem number =
  case number `quotRem` divisor of
    (q, 0) -> sayQuot q
    (q, r) -> sayQuot q `glue` sayRem r

integerLog1000 :: Integer -> Integer
integerLog1000 n = floor (logBase 10 (fromInteger n :: Double) / 3)

glueWith :: Semigroup a => a -> a -> a -> a
glueWith sep lhs rhs = lhs <> sep <> rhs
