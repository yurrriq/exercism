module PhoneNumber
  ( phoneNumber
  ) where

import Control.Bind (bindFlipped)
import Data.Array.NonEmpty (tail)
import Data.Function (($), flip, (<<<))
import Data.Functor (map)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.String (joinWith)
import Data.String.Regex (Regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (sequence)

-- | Given a string representing a valid North American telephone number, return
-- | `Just` a string of the digits, i.e. the area and exchange codes and the
-- | subscriber number. If the input is invalid, return `Nothing`.
phoneNumber :: String -> Maybe String
phoneNumber =
  map (joinWith "")
    <<< bindFlipped sequence
    <<< map tail
    <<< match nanp

-- The North American Numbering Plan (NANP)
nanp :: Regex
nanp =
  let
    separator = "(?:[.-]|\\s+)?"
  in
    flip unsafeRegex noFlags $
      -- optional country code
      "^\\s*(?:\\+?1\\s*)?"
        -- area code
        <> "\\(?([2-9]\\d{2})\\)?"
        <> separator
        -- exchange code
        <> "([2-9]\\d{2})"
        <> separator
        -- subscriber number
        <> "(\\d{4})\\s*$"
