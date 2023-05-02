module Diamond
  ( diamond,
  )
where

import Data.Char (chr, isUpper, ord)

diamond :: Char -> Maybe [String]
diamond z
  | isUpper z = Just (reflectRight (triangle z))
  | otherwise = Nothing

triangle :: Char -> [String]
triangle z = map (triangleRow (ord z)) [65 .. ord z]

triangleRow :: Int -> Int -> [Char]
triangleRow ordZ ordC =
  reflectLeft $
    replicate (ordC - 65) ' '
      <> (chr ordC : replicate (ordZ - ordC) ' ')

reflectLeft :: [a] -> [a]
reflectLeft = reverse <> tail

reflectRight :: [a] -> [a]
reflectRight = init <> reverse
