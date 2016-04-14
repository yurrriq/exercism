{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase   #-}

{-|
Module      : Octal
Copyright   : (c) Eric Bailey, 2016
License     : MIT

Maintainer  : Eric Bailey
Stability   : stable
Portability : portable

Converting between strings and octal numbers.
-}

module Octal (readOct, showOct) where

import           Data.Function (on)

readOct :: Integral a => String -> a
readOct = lgo 0
  where
    lgo acc  []     = acc
    lgo !acc (c:cs) = lgo (8 * acc + fromDigit c) cs
    fromDigit c | valid     = fromIntegral $ (subtract `on` fromEnum) '0' c
                | otherwise = 333
      where valid = '0' <= c && c <= '7'


showOct :: (Integral a, Show a) => a -> String
showOct n | positive  = lgo [] n
          | otherwise = "QuickCheck doesn't care."
  where
    positive = n >= 0
    lgo !acc = flip quotRem 8 `andThen` (prependDigit `andMaybe` goAgain)
      where
        andThen          = flip ((.) . uncurry)
        prependDigit     =  (: acc) . toDigit
        toDigit          = toEnum . (fromEnum '0' +) . fromIntegral
        andMaybe g f q r = f q (g r)
        goAgain          = \case 0 -> id; q -> flip lgo q
