{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Beer
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Lyrics to 99 Bottles of Beer on the Wall.
module Beer where

import Control.Monad (ap)
import Data.Bool (bool)
import Data.Char (toUpper)

-- | Given a number, returns the specified verse of the beer song.
verse :: Int -> String
verse = unlines . ([firstLine, secondLine] <*>) . pure
  where
    firstLine = \case
      0 -> (capitalize . bottles') 0 ++ ", " ++ bottles 0 ++ "."
      n -> bottles' n ++ ", " ++ bottles n ++ "."
    secondLine = \case
      0 -> buyMore $ bottles' 99 ++ "."
      n -> (passAround . takeDown) n ++ (bottles' . pred) n ++ "."
    buyMore = ("Go to the store and buy some more, " ++)
    takeDown = ("Take " ++) . (++ " down") . bool "one" "it" . (1 ==)
    passAround = (++ " and pass it around, ")

-- | Given @start@ and @end@ numbers, returns a string of all the 'verse's from
-- @start@ to @end@.
sing :: Int -> Int -> String
sing = (unlines . map verse) .: ap enumFromThenTo pred

-- sing start end = unlines $ map verse [start, pred start..end]

-- | Given a number, returns a string like @"99 bottles"@.
bottles :: Int -> String
bottles =
  (++ " of beer") . \case
    0 -> "no more bottles"
    1 -> "1 bottle"
    n -> show n ++ " bottles"

-- | Given a number, calls 'bottles' on it then appends @" on the wall"@.
bottles' :: Int -> String
bottles' = (++ " on the wall") . bottles

-- | Given an string, returns it with the first letter uppercased.
capitalize :: String -> String
capitalize = ap ((:) . toUpper . head) tail

-- | From "Data.Function.Pointless"
--
-- > (f .: g) x y = f (g x y)
--
-- or,
--
-- > f .: g = curry (f . uncurry g)
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.) . (.)
