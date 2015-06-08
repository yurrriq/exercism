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
  where lgo z []      = z
        lgo !z (x:xs) = lgo (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y = go
  where go [] = y
        go (x:xs) = f x $ go xs

length :: (Num b) => [a] -> b
length [] = 0
length (_:xs) = 1 + length xs

map :: (a -> b) -> [a] -> [b]
map f = foldr g []
  where g =  (:) . f

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []
