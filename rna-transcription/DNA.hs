{-# LANGUAGE LambdaCase #-}
{-|
Module      : DNA
Copyright   : (c) Eric Bailey, 2015
License     : MIT

Maintainer  : Eric Bailey
Stability   :  experimental
Portability :  portable

Transcription of DNA to RNA.
-}
module DNA (
  toRNA
  -- * Usage
  -- $usage
  ) where

-- | Given a DNA strand, returns its RNA complement (per RNA transcription).
toRNA :: String -> String
toRNA = map (\case 'G' -> 'C'; 'C' -> 'G'; 'T' -> 'A'; 'A' -> 'U')

{- $usage
>>> toRNA "A"
"U"
>>> toRNA "GATTACA"
"CUAAUGU"
-}
