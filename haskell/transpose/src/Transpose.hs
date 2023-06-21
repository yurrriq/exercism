{-# LANGUAGE CPP #-}

module Transpose
  ( transpose,
  )
where

-- NOTE: The test cases don't seem to follow the Reddit post.
-- I find the Exercism problem description confusing if not incorrect.
#ifdef REDDIT
import Data.List (dropWhileEnd)

transpose :: [String] -> [String]
transpose [] = []
transpose input =
  take width $
    map (dropWhileEnd (== ' ')) $
      go paddedInput
  where
    go xs = map head xs : go (map tail xs)
    paddedInput = map (padRight ' ' width) input
    width = maximum (map length input)
#else
import qualified Data.List as List

transpose :: [String] -> [String]
transpose [] = []
transpose input = List.transpose $ zipWith (padRight ' ') widths input
  where
    widths = scanr1 max (map length input)
#endif

padRight :: a -> Int -> [a] -> [a]
padRight x n xs = take n (xs ++ repeat x)
