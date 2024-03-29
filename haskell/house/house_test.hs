import House (rhyme)
import System.Exit (ExitCode (..), exitWith)
import Test.HUnit (Counts (..), Test (..), runTestTT, (~?=))

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $
    if failures counts /= 0 || errors counts /= 0
      then ExitFailure 1
      else ExitSuccess

houseTest :: Test
houseTest = rhyme ~?= expected

main :: IO ()
main = exitProperly $ runTestTT houseTest

expected :: String
expected =
  unlines
    [ "This is the house that Jack built.",
      "",
      "This is the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the man all tattered and torn",
      "that kissed the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the priest all shaven and shorn",
      "that married the man all tattered and torn",
      "that kissed the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the rooster that crowed in the morn",
      "that woke the priest all shaven and shorn",
      "that married the man all tattered and torn",
      "that kissed the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the farmer sowing his corn",
      "that kept the rooster that crowed in the morn",
      "that woke the priest all shaven and shorn",
      "that married the man all tattered and torn",
      "that kissed the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      "",
      "This is the horse and the hound and the horn",
      "that belonged to the farmer sowing his corn",
      "that kept the rooster that crowed in the morn",
      "that woke the priest all shaven and shorn",
      "that married the man all tattered and torn",
      "that kissed the maiden all forlorn",
      "that milked the cow with the crumpled horn",
      "that tossed the dog",
      "that worried the cat",
      "that killed the rat",
      "that ate the malt",
      "that lay in the house that Jack built.",
      ""
    ]
