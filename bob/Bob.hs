{-|
Module      : Bob
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   : experimental
Portability : portable

Mimicking the conversational ineptitude of a lackadaisical teenager.
-}

module Bob (responseFor) where

import           Control.Monad (ap)
import           Data.Char     (isSpace, isUpper, toUpper)

-- | Given a prompt ('String'), returns a teenager's response ('String').
responseFor :: String -> String
responseFor s
  | isYelled s   = "Whoa, chill out!"
  | isSilent s   = "Fine. Be that way!"
  | isQuestion s = "Sure."
  | otherwise    = "Whatever."
    where isYelled   = ap ((&&) . any isUpper) ((==) =<< map toUpper)
          isSilent   = (==) =<< filter isSpace
          isQuestion = ('?' ==) . last
