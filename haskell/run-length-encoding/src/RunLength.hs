module RunLength (decode, encode) where

import qualified Data.Char as Char
import qualified Data.List as List


decode :: String -> String
decode = go []
  where
    go :: [String] -> String -> String
    go acc ""  = reverse (concat acc)
    go acc str = let (digits, c : cs) = span Char.isDigit str
                     n = case reads digits :: [(Int, String)] of
                           []       -> 1
                           [(m, _)] -> m
                 in  go (replicate n c : acc) cs


encode :: String -> String
encode = concatMap go . List.group
  where
    go :: String -> String
    go ""     = ""
    go [c]    = [c]
    go (c:cs) = show (1 + length cs) ++ [c]
