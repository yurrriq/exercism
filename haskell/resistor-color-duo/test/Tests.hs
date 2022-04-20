{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import Data.Foldable (for_)
import ResistorColors (Color (..), value)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)
import Test.QuickCheck (Gen, elements, forAll, suchThat)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "value" $
  do
    describe "equality tests" $ for_ cases test
    describe "property tests" $
      do
        it "all values starting with Black are single digit" $
          forAll colorGen (\color -> value (Black, color) < 10)
        it "all values not starting with Black are double digits" $
          forAll (colorsGen `suchThat` ((/=) Black . fst)) $ \colors ->
            value colors >= 10
        it "all colors have unique names" $
          forAll (colorsGen `suchThat` uncurry (/=)) $ \(x, y) ->
            show x /= show y
  where
    test Case {..} = it explanation assertion
      where
        explanation = unwords [show input, "-", description]
        assertion = value input `shouldBe` expected

colorGen :: Gen Color
colorGen = elements [minBound ..]

colorsGen :: Gen (Color, Color)
colorsGen = (,) <$> colorGen <*> colorGen

data Case = Case
  { description :: String,
    input :: (Color, Color),
    expected :: Int
  }

cases :: [Case]
cases =
  [ Case
      { description = "Brown and black",
        input = (Brown, Black),
        expected = 10
      },
    Case
      { description = "Blue and grey",
        input = (Blue, Grey),
        expected = 68
      },
    Case
      { description = "Yellow and violet",
        input = (Yellow, Violet),
        expected = 47
      },
    Case
      { description = "Orange and orange",
        input = (Orange, Orange),
        expected = 33
      }
      -- Note: This test suite omits testing three-color bands,
      -- since they are not representable as (Color, Color). They
      -- are addressed in the exercise resistor-color-trio.
  ]

-- c193c935fe902d4004778872de9e4e61108c271a
