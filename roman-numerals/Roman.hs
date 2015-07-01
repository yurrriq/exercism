module Roman (numerals) where

type Arabic = Int
type Roman  = String

translations :: [(Arabic, Roman)]
translations = [(1000,  "M"),
                (900,  "CM"),
                (500,   "D"),
                (400,  "CD"),
                (100,   "C"),
                (90,   "XC"),
                (50,    "L"),
                (40,   "XL"),
                (10,    "X"),
                (9,    "IX"),
                (5,     "V"),
                (4,    "IV"),
                (1,     "I")]

numerals :: Arabic -> Roman
numerals n = go n "" translations
  where go :: Arabic -> String -> [(Arabic, Roman)] -> Roman
        go x r' ts
          | null ts   = r'
          | m > 0     = go (x-a*m) (r' ++ concat (replicate m r)) (tail ts)
          | otherwise = go x r' (tail ts)
            where (a,r) = head ts
                  m     = x `div` a

-- Local Variables:
-- compile-command: "runhaskell roman-numerals_test.hs"
-- End:
