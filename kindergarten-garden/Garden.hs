{-# LANGUAGE LambdaCase #-}

module Garden (Plant(..), defaultGarden, garden, lookupPlants) where

import           Data.List       (elemIndex, sort)
import           Data.List.Split (chunksOf)
import           Data.Map        (Map)
import qualified Data.Map        as M
import           Data.Maybe      (mapMaybe)

data Plant   = Grass
             | Clover
             | Radishes
             | Violets
             deriving (Bounded, Enum, Eq, Read, Show)

type Layout  = String

type Student = String

type Garden  = Map Student [Plant]

defaultGarden :: Layout -> Garden
defaultGarden = garden classList

garden :: [Student] -> Layout -> Garden
garden students = M.fromListWith (++) .
                  concatMap (sortZip students . kidRows) .
                  reverse . linesMap fromString
  where
    linesMap = (. lines) . map
    kidRows  = chunksOf 2
    sortZip  = zip . sort

lookupPlants :: Student -> Garden -> [Plant]
lookupPlants = M.findWithDefault []

fromString :: String -> [Plant]
fromString = mapMaybe $ (toEnum <$>) . (`elemIndex` "GCRV")

classList :: [Student]
classList = ["Alice",  "Bob",    "Charlie", "David",
             "Eve",    "Fred",   "Ginny",   "Harriet",
             "Ileana", "Joseph", "Kincaid", "Larry"]
