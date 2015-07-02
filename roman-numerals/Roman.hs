module Roman (numerals) where

type Arabic = Int
type Roman  = String

conversions :: [(Arabic, Roman)]
conversions = [(1000,  "M"), (900, "CM"),
               (500,   "D"), (400, "CD"),
               (100,   "C"), (90,  "XC"),
               (50,    "L"), (40,  "XL"),
               (10,    "X"), (9,   "IX"),
               (5,     "V"), (4,   "IV"),
               (1,     "I")]

numerals :: Arabic -> Roman
numerals = go ("" ++) conversions
  where go :: ShowS -> [(Arabic, Roman)] -> Arabic -> Roman
        go f [] _ = f ""
        go f ((a, r):cs') x
          | x >= a     = go f' cs' x'
          | otherwise = go f cs' x
          where (n, x') = x `divMod` a
                f'      = f . (concat (replicate n r) ++)


-- Local Variables:
-- compile-command: "runhaskell roman-numerals_test.hs"
-- End:
