module Bob (responseFor) where

import Control.Monad (ap)
import Data.Char (isSpace, isUpper, toUpper)

responseFor :: String -> String
responseFor s
  | isYelled s   = "Whoa, chill out!"
  | isSilent s   = "Fine. Be that way!"
  | isQuestion s = "Sure."
  | otherwise    = "Whatever."
    where isYelled   = ap ((&&) . any isUpper) ((==) =<< map toUpper)
          isSilent   = (==) =<< filter isSpace
          isQuestion = ('?' ==) . last
