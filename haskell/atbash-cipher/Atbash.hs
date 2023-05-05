module Atbash (encode) where

import Data.Bool (bool)
import Data.Char (chr, isDigit, toLower)
import Data.Function (on)
import Data.Ix (inRange)
import Data.List (unfoldr)
import Data.Maybe (mapMaybe)

encode :: String -> String
encode = unwords . partitionAll 5 . mapMaybe cipher

cipher :: Char -> Maybe Char
cipher = bool (rotate . toLower) Just =<< isDigit

rotate :: Char -> Maybe Char
rotate = bool (const Nothing) rotate' =<< inRange ('a', 'z')
  where
    rotate' = Just . chr . (fromEnum 'z' -) . (flip mod `on` fromEnum) 'a'

-- | Like Clojure's @partition-all@.
partitionAll :: Int -> [a] -> [[a]]
partitionAll n xs = unfoldr (go . splitAt n) xs
  where
    go ([], []) = Nothing
    go pair = Just pair

-- partitionAll n xs = takeWhile (not . null) $ unfoldr (Just . splitAt n) xs
