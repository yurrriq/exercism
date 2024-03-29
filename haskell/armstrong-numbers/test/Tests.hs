{-# LANGUAGE RecordWildCards #-}

import ArmstrongNumbers (armstrong)
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFailFast, defaultConfig, hspecWith)

main :: IO ()
main = hspecWith defaultConfig {configFailFast = True} specs

specs :: Spec
specs = describe "armstrong" $ for_ cases test
  where
    test Case {..} = it description assertion
      where
        assertion = armstrong input `shouldBe` expected

data Case = Case
  { description :: String,
    input :: Int,
    expected :: Bool
  }

cases :: [Case]
cases =
  [ Case
      { description = "Zero is an Armstrong numbers",
        input = 0,
        expected = True
      },
    Case
      { description = "Single digit numbers are Armstrong numbers",
        input = 5,
        expected = True
      },
    Case
      { description = "There are no 2 digit Armstrong numbers",
        input = 10,
        expected = False
      },
    Case
      { description = "Three digit number that is an Armstrong number",
        input = 153,
        expected = True
      },
    Case
      { description = "Three digit number that is not an Armstrong number",
        input = 100,
        expected = False
      },
    Case
      { description = "Four digit number that is an Armstrong number",
        input = 9474,
        expected = True
      },
    Case
      { description = "Four digit number that is not an Armstrong number",
        input = 9475,
        expected = False
      },
    Case
      { description = "Seven digit number that is an Armstrong number",
        input = 9926315,
        expected = True
      },
    Case
      { description = "Seven digit number that is not an Armstrong number",
        input = 9926314,
        expected = False
      }
  ]
