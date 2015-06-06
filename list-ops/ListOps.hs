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

import Prelude hiding ((++), concat, filter, foldr, length, map, reverse)

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where f x = if' (p x) =<< (x :)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = lgo
  where lgo z []      = z
        lgo !z (x:xs) = lgo (f z x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y = go
  where go [] = y
        go (x:xs) = f x $ go xs

length :: [a] -> Int
length = foldr (const (1 +)) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr g []
  where g =  (:) . f

reverse :: [a] -> [a]
reverse xs = foldr f id xs []
  where f = flip (.) . (:)
