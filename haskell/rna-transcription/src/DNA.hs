{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : DNA
-- Copyright   : (c) Eric Bailey, 2015-2023
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Transcription of DNA to RNA.
module DNA
  ( toRNA,
  )
where

-- | Given a DNA strand, return either the first invalid nucleotide found or the
-- strand's RNA complement (per RNA transcription).
--
-- >>> toRNA "A"
-- Right "U"
-- >>> toRNA "GATTACA"
-- Right "CUAAUGU"
-- >>> toRNA "ACGTXXXCTTAA"
-- Left 'X'
toRNA :: String -> Either Char String
toRNA = mapM $ \case
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  x -> Left x
