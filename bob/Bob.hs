module Bob (responseFor) where

import Prelude hiding (null)
import Control.Monad (ap)
import Data.Char (isUpper, toUpper)
import Data.Text (null, pack, strip)

responseFor :: String -> String
responseFor prompt
  | isYelled   prompt = "Whoa, chill out!"
  | isSilent   prompt = "Fine. Be that way!"
  | isQuestion prompt = "Sure."
  | otherwise         = "Whatever."
  where isYelled      = ap ((&&) . any isUpper) ((==) =<< map toUpper)
        isSilent      = null . strip . pack
        isQuestion    = ('?' ==) . last
