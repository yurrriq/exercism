-- |
-- Module      : OCR
-- Copyright   : (c) Eric Bailey, 2016
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : stable
-- Portability : portable
--
-- Parsing 3 x 4 grids of pipes, underscores, and spaces for represented digits.
module OCR (Grid, Digit, convert) where

import Data.List (intercalate, transpose)
import Data.List.Split (chunksOf)
import qualified Data.Map as M

-- | A 'Grid' is a list of four rows, which are three-character-long 'String's,
-- made up of 'Char's that are one of @[' ', '|', '_']@. For example:
--
-- @
-- three :: Grid
-- three = [ " _ "
--         , " _|"
--         , " _|"
--         , "   " ]
-- @
type Grid = [String]

-- | A 'Digit' is one of @['0'..'9']@ or @'?'@.
type Digit = Char

-- | Given a 'String' representing one or more lines of 'Grid's,
-- 'convert' it to a 'String' of 'Digit's, using @','@ as a grid line separator.
-- For example:
--
-- @
-- let example = unlines [ " _  _  _ "
--                       , " _| _| _|"
--                       , " _| _| _|"
--                       , "         " ]
--   in convert example == "333"
-- @
convert :: String -> String
convert = intercalate "," . map lineToDigits . toLines
  where
    lineToDigits = map parseToGrid
    parseToGrid = flip (M.findWithDefault '?') gridToDigit
    toLines = rowsToLines . allRows . lines
    rowsToLines = map (transpose . map allCols)
    allCols = chunksOf 3
    allRows = chunksOf 4

gridToDigit :: M.Map Grid Digit
gridToDigit = M.fromList $ flip zip digits $ grids
  where
    digits = ['0' .. '9']
    grids =
      [ [ " _ ",
          "| |",
          "|_|",
          "   "
        ],
        [ "   ",
          "  |",
          "  |",
          "   "
        ],
        [ " _ ",
          " _|",
          "|_ ",
          "   "
        ],
        [ " _ ",
          " _|",
          " _|",
          "   "
        ],
        [ "   ",
          "|_|",
          "  |",
          "   "
        ],
        [ " _ ",
          "|_ ",
          " _|",
          "   "
        ],
        [ " _ ",
          "|_ ",
          "|_|",
          "   "
        ],
        [ " _ ",
          "  |",
          "  |",
          "   "
        ],
        [ " _ ",
          "|_|",
          "|_|",
          "   "
        ],
        [ " _ ",
          "|_|",
          " _|",
          "   "
        ]
      ]
