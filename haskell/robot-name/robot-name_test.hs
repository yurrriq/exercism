import Robot (mkRobot, resetName, robotName)
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit
  ( Assertion,
    Counts (..),
    Test (..),
    runTestTT,
    (@=?),
    (@?),
  )
import Text.Regex (mkRegex)
import Text.Regex.Base (matchTest)

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main =
  exitProperly $
    runTestTT $
      TestList
        [TestList robotTests]

{-
These tests of course *can* fail since we are expected to use a random number
generator. The chances of this kind of failure are very small. A
real "robot generator" would use a proper serial number system and
would likely not be in the business of resetting the name.
-}
robotTests :: [Test]
robotTests =
  [ testCase "name should match expected pattern" $
      matchesPattern
        <$> (mkRobot >>= robotName)
        @? "name did not match expected pattern",
    testCase "name is persistent" $ do
      r <- mkRobot
      n1 <- robotName r
      n2 <- robotName r
      n3 <- robotName r
      n1 @=? n2
      n1 @=? n3,
    testCase "different robots have different names" $ do
      n1 <- mkRobot >>= robotName
      n2 <- mkRobot >>= robotName
      n1 /= n2 @? "different robots should have different names",
    testCase "new name should match expected pattern" $ do
      r <- mkRobot
      resetName r
      matchesPattern <$> robotName r @? "name did not match expected pattern",
    testCase "new name is persistent" $ do
      r <- mkRobot
      resetName r
      n1 <- robotName r
      n2 <- robotName r
      n3 <- robotName r
      n1 @=? n2
      n1 @=? n3,
    testCase "new name is different from old name" $ do
      r <- mkRobot
      n1 <- robotName r
      resetName r
      n2 <- robotName r
      n1 /= n2 @? "name should change when reset",
    testCase "resetting a robot affects only one robot" $ do
      r1 <- mkRobot
      r2 <- mkRobot
      n1 <- robotName r1
      n2 <- robotName r2
      n1 /= n2 @? "different robots should have different names"
      resetName r1
      n1' <- robotName r1
      n2' <- robotName r2
      n1' /= n2' @? "names should be different"
      n2 @=? n2'
  ]

matchesPattern :: String -> Bool
matchesPattern = matchTest pattern
  where
    pattern = mkRegex "^[A-Z]{2}[0-9]{3}$"
