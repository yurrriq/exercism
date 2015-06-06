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

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = undefined

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr = undefined

length :: [a] -> Int
length = undefined

reverse :: [a] -> [a]
reverse = undefined

map :: (a -> b) -> [a] -> [b]
map = undefined

filter :: (a -> Bool) -> [a] -> [a]
filter = undefined

(++) :: [a] -> [a] -> [a]
xs ++ ys = undefined

concat :: [[a]] -> [a]
concat = undefined
