module ProteinTranslation
  ( proteins,
  )
where

import Data.List.Split (chunksOf)

proteins :: String -> Maybe [String]
proteins [] = Just []
proteins codons = go (Just []) (chunksOf 3 codons)
  where
    go (Just protein) [] = Just (reverse protein)
    go (Just protein) (codon : codons') =
      case codon of
        "AUG" -> go (Just ("Methionine" : protein)) codons'
        "UUU" -> go (Just ("Phenylalanine" : protein)) codons'
        "UUC" -> go (Just ("Phenylalanine" : protein)) codons'
        "UUA" -> go (Just ("Leucine" : protein)) codons'
        "UUG" -> go (Just ("Leucine" : protein)) codons'
        "UCU" -> go (Just ("Serine" : protein)) codons'
        "UCC" -> go (Just ("Serine" : protein)) codons'
        "UCA" -> go (Just ("Serine" : protein)) codons'
        "UCG" -> go (Just ("Serine" : protein)) codons'
        "UAU" -> go (Just ("Tyrosine" : protein)) codons'
        "UAC" -> go (Just ("Tyrosine" : protein)) codons'
        "UGU" -> go (Just ("Cysteine" : protein)) codons'
        "UGC" -> go (Just ("Cysteine" : protein)) codons'
        "UGG" -> go (Just ("Tryptophan" : protein)) codons'
        "UAA" -> Just (reverse protein)
        "UAG" -> Just (reverse protein)
        "UGA" -> Just (reverse protein)
        _ -> Nothing
    go _ _ = Nothing
