{-# LANGUAGE ApplicativeDo #-}

module Triangle
  ( rows,
  )
where

import Control.Applicative (ZipList (..))

rows :: Int -> [[Integer]]
rows n = take n triangle

triangle :: [[Integer]]
triangle = iterate go [1]
  where
    go :: [Integer] -> [Integer]
    go xs =
      getZipList $ do
        x <- ZipList (0 : xs)
        y <- ZipList (xs ++ [0])
        return (x + y)

{-

{-# LANGUAGE ParallelListComp #-}

triangle :: [[Integer]]
triangle = iterate go [1]
  where
    go :: [Integer] -> [Integer]
    go xs = [x + y | x <- 0 : xs | y <- xs ++ [0]]

-}
