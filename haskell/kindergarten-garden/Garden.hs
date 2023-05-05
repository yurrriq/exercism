-- |
-- Module      : Garden
-- Copyright   : (c) Eric Bailey, 2015
-- License     : MIT
--
-- Maintainer  : Eric Bailey
-- Stability   : experimental
-- Portability : portable
--
-- Parsing kindergarten garden diagrams.
module Garden (Plant (..), defaultGarden, garden, lookupPlants) where

import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

data Plant
  = Grass
  | Clover
  | Radishes
  | Violets
  deriving (Bounded, Enum, Eq, Read, Show)

-- | A diagram is a string.
type Diagram = String

-- | A student('s first name) is a string.
type Student = String

-- | A garden is a map from 'Student' to a list of 'Plants'.
type Garden = Map Student [Plant]

-- | Call 'garden' with 'classList' and return the default 'Garden'.
defaultGarden :: Diagram -> Garden
defaultGarden = garden classList

-- | Given a list of 'Student's and a 'Diagram',
-- return the 'Garden' represented by the the given 'Garden'.
garden :: [Student] -> Diagram -> Garden
garden students =
  M.fromListWith (++)
    . concatMap (sortZip students . kidRows)
    . reverse
    . linesMap fromString
  where
    linesMap = (. lines) . map
    kidRows = chunksOf 2
    sortZip = zip . sort

-- | Given a 'Student' and a 'Garden', return the list of 'Plant's
-- belonging to the given 'Student'.
lookupPlants :: Student -> Garden -> [Plant]
lookupPlants = M.findWithDefault []

-- Given a string of first letters of 'Plant' names,
-- return the list of 'Plant's it represents.
fromString :: String -> [Plant]
fromString = mapMaybe $ (toEnum <$>) . (`elemIndex` "GCRV")

-- The default list of 'Student's.
classList :: [Student]
classList =
  [ "Alice",
    "Bob",
    "Charlie",
    "David",
    "Eve",
    "Fred",
    "Ginny",
    "Harriet",
    "Ileana",
    "Joseph",
    "Kincaid",
    "Larry"
  ]
