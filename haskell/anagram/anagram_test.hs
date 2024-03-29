import Anagram (anagramsFor)
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit (Assertion, Counts (..), Test (..), runTestTT, (@=?))

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly (runTestTT (TestList anagramTests))

anagramTests :: [Test]
anagramTests =
  [ testCase "no matches" $
      [] @=? anagramsFor "diaper" ["hello", "world", "zombies", "pants"],
    testCase "detect simple anagram" $
      ["tan"] @=? anagramsFor "ant" ["tan", "stand", "at"],
    testCase "does not confuse different duplicates" $
      [] @=? anagramsFor "galea" ["eagle"],
    testCase "eliminate anagram subsets" $
      [] @=? anagramsFor "good" ["dog", "goody"],
    testCase "detect anagram" $
      ["inlets"]
        @=? anagramsFor
          "listen"
          [ "enlists",
            "google",
            "inlets",
            "banana"
          ],
    testCase "multiple anagrams" $
      ["gallery", "regally", "largely"]
        @=? anagramsFor
          "allergy"
          [ "gallery",
            "ballerina",
            "regally",
            "clergy",
            "largely",
            "leading"
          ],
    testCase "case insensitive anagrams" $
      ["Carthorse"]
        @=? anagramsFor "Orchestra" ["cashregister", "Carthorse", "radishes"],
    testCase "does not detect a word as its own anagram" $
      [] @=? anagramsFor "banana" ["banana"],
    testCase "does not detect a word as its own anagram (case insensitive)" $
      [] @=? anagramsFor "Banana" ["baNana"]
  ]
