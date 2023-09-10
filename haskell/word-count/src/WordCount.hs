module WordCount (wordCount) where

import Control.Applicative (liftA2)
import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (dropWhileEnd, foldl')
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
words' =
  map unQuote
    . wordsBy (not . (isAlphaNum <||> isQuote))

-- | Given a string, returns a Map from word to frequency for each word.
wordCount :: String -> Map String Int
wordCount = foldl' go Map.empty . map lowercase . words'
  where
    go seen word = Map.insertWith (+) word 1 seen

unQuote :: String -> String
unQuote = dropWhileEnd isQuote . dropWhile isQuote

isQuote :: Char -> Bool
isQuote '\'' = True
isQuote _ = False

(<||>) :: (Applicative f) => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)

infixr 2 <||>
