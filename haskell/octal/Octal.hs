{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : Octal
-- Copyright   : (c) Eric Bailey, 2016
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Converting between strings and octal numbers.
module Octal (readOct, showOct) where

import Control.Arrow (first, (&&&), (***), (>>>))
import Control.Monad (ap, liftM2)
import Data.Bool (bool)
import Data.Function (on)
import Data.Function.Pointless ((.:))
import Data.List (uncons)

readOct :: Integral a => String -> a
readOct = lgo 0
  where
    lgo !acc = maybe acc go . uncons
      where
        go = uncurry lgo . first (f acc)
          where
            f = curry $ (8 *) *** fromDigit >>> uncurry (+)

fromDigit :: Integral a => Char -> a
fromDigit = boolAp (badInput "Invalid digit") go valid
  where
    go = fromIntegral . (subtract `on` fromEnum) '0'
    valid = ('0' <=) &&& (<= '7') >>> uncurry (&&)

showOct :: (Integral a, Show a) => a -> String
showOct = boolAp (badInput "Negative number") (`rgo` "") (>= 0)
  where
    rgo :: (Integral a, Show a) => a -> String -> String
    rgo x !acc = boolAp g f p (quotRem x 8)
      where
        p = (== 0) . fst
        f = snd >>> toDigit >>> (: acc)
        g = liftM2 rgo fst f

toDigit :: Integral a => a -> Char
toDigit = toEnum . (fromEnum '0' +) . fromIntegral

boolAp :: (a -> b) -> (a -> b) -> (a -> Bool) -> a -> b
boolAp = ap .: liftM2 bool

badInput :: Show a => String -> a -> t
badInput reason input = error (reason ++ ": " ++ show input)
