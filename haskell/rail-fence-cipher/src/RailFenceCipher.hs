module RailFenceCipher
  ( encode,
    decode,
  )
where

import Data.List (sortOn)

-- | Encode a message using a rail fence cipher with a given number of rails.
encode :: Int -> [a] -> [a]
encode numRails = zipSort (zigZag numRails)

-- | Decode a rail fence ciphertext with a given number of rails.
decode :: Int -> [a] -> [a]
decode 1 message = message
decode numRails ciphertext = zipSort indices ciphertext
  where
    indices = encode numRails [1 .. length ciphertext]

-- | Sort a list by its corresponding indices.
--
-- > zipSort [3,5,2,4,1] ['a'..] == "ecadb"
zipSort :: (Ord b) => [b] -> [a] -> [a]
zipSort indices list = map snd (sortOn fst (zip indices list))

-- | The list @[1 .. n] ++ [n - 1, n - 2 .. 2]@ repeated.
--
-- > take 8 (zigZag 1) == [1, 1, 1, 1, 1, 1, 1, 1]
-- > take 8 (zigZag 2) == [1, 2, 1, 2, 1, 2, 1, 2]
-- > take 8 (zigZag 4) == [1, 2, 3, 4, 3, 2, 1, 2]
zigZag :: Int -> [Int]
zigZag 1 = repeat 1
zigZag 2 = cycle [1, 2]
zigZag n
  | n > 0 = cycle (xs ++ tail (reverse (tail xs)))
  | otherwise = []
  where
    xs = [1 .. n]
