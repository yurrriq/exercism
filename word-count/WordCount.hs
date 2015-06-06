module WordCount where

import Data.Char (isAlphaNum, toLower)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

wordCount :: String -> Map String Int
wordCount = foldr (f . filter isAlphaNum) Map.empty . words'
  where f []     = id
        f x      = Map.insertWith (+) (map toLower x) 1
        words' s = case dropWhile p s of
                        "" -> []
                        s' -> w : words' s'' where (w, s'') = break p s'
          where p = not . isAlphaNum
