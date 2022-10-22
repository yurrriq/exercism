module IsbnVerifier
  ( isbn,
  )
where

import Data.Char (digitToInt)

isbn :: String -> Bool
isbn = go 10 0
  where
    go 0 checksum [] =
      checksum `rem` 11 == 0
    go pos checksum ('-' : chars) =
      go pos checksum chars
    go 1 checksum "X" =
      go 0 (checksum + 10) []
    go pos checksum (char : chars)
      | char `elem` "0123456789" =
        go (pos - 1) (checksum + (digitToInt char * pos)) chars
    go _ _ _ = False
