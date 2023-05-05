{-# LANGUAGE BangPatterns #-}

-- |
-- Module      : ListOpts
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Basic list operations.
module ListOps
  ( (++),
    concat,
    filter,
    foldl',
    foldr,
    length,
    map,
    reverse,
  )
where

import Control.Monad (ap)
import Data.Bool (bool)
import Prelude hiding
  ( concat,
    filter,
    foldr,
    length,
    map,
    reverse,
    (++),
  )

-- | DIY 'Data.List.(++)'
(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

-- | DIY 'Data.List.concat'
concat :: [[a]] -> [a]
concat = foldr (++) []

-- | DIY 'Data.List.filter'
filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr f []
  where
    f = ap (bool id . (:)) p

-- | DIY 'Data.List.foldl''
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f = lgo
  where
    lgo acc [] = acc
    lgo !acc (x : xs) = lgo (f acc x) xs

-- | DIY 'Data.List.foldr'
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f y = go
  where
    go [] = y
    go (x : xs) = f x $ go xs

-- | DIY 'Data.List.length'
--
-- Illustration of the fold
--
-- @
--  length = go 0
--   where go acc []      = acc
--         go !acc (_:xs) = go (acc + 1) xs
-- @
length :: (Num b) => [a] -> b
length = foldl' (const . (1 +)) 0

-- | DIY 'Data.List.map'
map :: (a -> b) -> [a] -> [b]
map f = foldr g []
  where
    g = (:) . f

-- | DIY 'Data.List.reverse'
reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []
