module RailFenceCipher
  ( encode,
    decode,
  )
where

import Data.Char (isAlpha)
import Data.List (sortOn)

encode :: Int -> String -> String
encode n = railFence n . filter isAlpha

decode :: Int -> String -> String
decode = unRailFence

railFence :: Int -> [a] -> [a]
railFence n = sortIndexed . zip (zigZag n)

unRailFence :: Int -> [b] -> [b]
unRailFence n str = sortIndexed (zip (railFence n [1 .. length str]) str)

sortIndexed :: [(Int, b)] -> [b]
sortIndexed = map snd . sortOn fst

zigZag :: (Num a, Enum a) => a -> [a]
zigZag n = cycle ([1 .. n] ++ [n - 1, n - 2 .. 2])
