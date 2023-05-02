module Diamond
  ( diamond,
  )
where

import Data.Char (chr, isUpper, ord)
import Data.Text (Text)
import qualified Data.Text as Text

diamond :: Char -> Maybe [Text]
diamond z
  | isUpper z = Just (reflectRight (triangle z))
  | otherwise = Nothing

triangle :: Char -> [Text]
triangle z = map (triangleRow (ord z)) [65 .. ord z]

triangleRow :: Int -> Int -> Text
triangleRow ordZ ordC =
  Text.pack . reflectLeft $
    replicate (ordC - 65) ' '
      <> (chr ordC : replicate (ordZ - ordC) ' ')

reflectLeft :: [a] -> [a]
reflectLeft = reverse <> tail

reflectRight :: [a] -> [a]
reflectRight = init <> reverse
