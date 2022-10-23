module ProteinTranslation
  ( proteins,
  )
where

import Data.List.Split (chunksOf)
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Map

proteins :: String -> Maybe [String]
proteins = fmap (takeWhile (/= "STOP")) . mapM (translations !?) . chunksOf 3

translations :: Map String String
translations =
  Map.fromList
    . concatMap (\(codons, protein) -> [(codon, protein) | codon <- codons])
    $ [ (["AUG"], "Methionine"),
        (["UGG"], "Tryptophan"),
        (["UUU", "UUC"], "Phenylalanine"),
        (["UUA", "UUG"], "Leucine"),
        (["UCU", "UCC", "UCA", "UCG"], "Serine"),
        (["UAU", "UAC"], "Tyrosine"),
        (["UGU", "UGC"], "Cysteine"),
        (["UAA", "UAG", "UGA"], "STOP")
      ]
