module Series
  ( slices,
  )
where

import Data.Char (digitToInt)
import Data.List (tails)
import Data.Maybe (mapMaybe)
import Safe.Exact (takeExactMay)

slices :: Int -> String -> [[Int]]
slices n xs =
  mapMaybe (map digitToInt <.> takeExactMay n) $
    tails xs

infixr 9 <.>

(<.>) :: Functor m => (b -> c) -> (a -> m b) -> a -> m c
(f <.> g) a = f <$> g a
