{-# LANGUAGE BangPatterns #-}
module ListOps (
  (++),
  concat,
  filter,
  foldl',
  foldr,
  length,
  map,
  reverse
  ) where

import Control.Monad (ap)
import Data.Bool (bool)
import Prelude hiding ((++), concat, filter, foldr, length, map, reverse)

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where f = ap (bool id . (:)) p

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = lgo
  where lgo acc []      = acc
        lgo !acc (x:xs) = lgo (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y = go
  where go [] = y
        go (x:xs) = f x $ go xs

-- | Illustration of the fold
--
-- @
--  length = go 0
--   where go acc []      = acc
--         go !acc (_:xs) = go (acc + 1) xs
-- @
--
length :: (Num b) => [a] -> b
length = foldl' (const . (1 +)) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr g []
  where g =  (:) . f

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []
