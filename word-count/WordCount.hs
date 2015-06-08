module WordCount (wordCount) where

import Data.Char (isAlphaNum, toLower)
import Data.List.Split (wordsBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Given a string, returns a string with each character converted
-- to lowercase. Also found in "Distribution.Simple.Utils".
lowercase :: String -> String
lowercase = map toLower

-- | Given a string, returns a list of strings by splitting on (and dropping)
-- non-alphanumeric characters.
words' :: String -> [String]
words' = wordsBy $ not . isAlphaNum

-- | Given a string, returns a Map from word to frequency for each word.
wordCount :: String -> Map String Int
wordCount = Map.fromListWith (+) . toPairs . words'
  where toPairs :: [String] -> [(String, Int)]
        toPairs = map $ flip (,) 1 . lowercase
