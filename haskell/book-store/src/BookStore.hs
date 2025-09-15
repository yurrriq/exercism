module BookStore
  ( Book (..),
    total,
  )
where

import Algorithm.Search (dijkstra)
import Data.IntMultiSet (IntMultiSet, (\\))
import Data.IntMultiSet qualified as IMS

data Book
  = First
  | Second
  | Third
  | Fourth
  | Fifth
  deriving (Eq, Enum, Show)

total :: [Book] -> Int
total =
  maybe 0 fst
    . dijkstra purchases cost IMS.null
    . fromBooks
  where
    purchases basket = map (basket \\) allSets
    cost from to =
      case IMS.distinctSize (from \\ to) of
        1 -> 800
        2 -> 1520
        3 -> 2160
        4 -> 2560
        5 -> 3000
        k ->
          error $
            unlines
              [ "Invalid number of different books: " <> show k,
                "Must be one of 1, 2, 3, 4, or 5."
              ]

allSets :: [IntMultiSet]
allSets =
  IMS.fromDistinctAscList [0 .. 4]
    : concatMap
      (map IMS.fromDistinctAscList . flip choose [0 .. 4])
      [2, 3, 4]

-- | All \(k\)-combinations of the natural numbers \(1\) through \(n\).
choose :: Int -> [Int] -> [[Int]]
choose 0 _ = [[]]
choose _ [] = []
choose k (x : xs) = map (return x `mappend`) (choose (k - 1) xs) ++ choose k xs

fromBooks :: [Book] -> IntMultiSet
fromBooks = IMS.fromList . map fromEnum
