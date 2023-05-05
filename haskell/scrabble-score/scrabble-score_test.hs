import Scrabble (scoreLetter, scoreWord)
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit (Assertion, Counts (..), Test (..), runTestTT, (@=?))

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
        [TestList scrabbleTests]

scrabbleTests :: [Test]
scrabbleTests =
  map
    TestCase
    [ 1 @=? scoreLetter 'a',
      1 @=? scoreLetter 'A',
      2 @=? scoreWord "at",
      6 @=? scoreWord "street",
      22 @=? scoreWord "quirky",
      20 @=? scoreWord "MULTIBILLIONAIRE"
    ]
