module ProteinTranslation
  ( proteins,
  )
where

import Data.List.Split (chunksOf)

proteins :: String -> Maybe [String]
proteins = fmap (takeWhile (/= "STOP")) . mapM go . chunksOf 3
  where
    go "AUG" = Just "Methionine"
    go "UGG" = Just "Tryptophan"
    go codon
      | codon `elem` ["UUU", "UUC"] = Just "Phenylalanine"
      | codon `elem` ["UUA", "UUG"] = Just "Leucine"
      | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
      | codon `elem` ["UAU", "UAC"] = Just "Tyrosine"
      | codon `elem` ["UGU", "UGC"] = Just "Cysteine"
      | codon `elem` ["UAA", "UAG", "UGA"] = Just "STOP"
    go _ = Nothing
