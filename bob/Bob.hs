{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Bob (responseFor) where

import Control.Monad (ap)
import Data.Char (isSpace, isUpper, toUpper)

responseFor :: String -> String
responseFor = \case
  ((isYelled)   -> True) -> "Whoa, chill out!"
  ((isSilent)   -> True) -> "Fine. Be that way!"
  ((isQuestion) -> True) -> "Sure."
  _                      -> "Whatever."
  where isYelled   = ap ((&&) . any isUpper) ((==) =<< map toUpper)
        isSilent   = (==) =<< filter isSpace
        isQuestion = ('?' ==) . last
