import           Data.Time.Clock  (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeOrError)
import           Gigasecond       (fromDay)
import           System.Exit      (ExitCode (..), exitWith)
import           Test.HUnit       (Assertion, Counts (..), Test (..), runTestTT,
                                   (@=?))

dt :: String -> UTCTime
dt = parseTimeOrError True defaultTimeLocale "%FT%T%Z"

exitProperly :: IO Counts -> IO ()
exitProperly m = do
  counts <- m
  exitWith $ if failures counts /= 0 || errors counts /= 0 then ExitFailure 1 else ExitSuccess

testCase :: String -> Assertion -> Test
testCase label assertion = TestLabel label (TestCase assertion)

main :: IO ()
main = exitProperly $ runTestTT $ TestList
       [ TestList gigasecondTests ]

gigasecondTests :: [Test]
gigasecondTests =
  [ testCase "from apr 25 2011" $
    dt "2043-01-01T01:46:40Z" @=? fromDay (dt "2011-04-25T00:00:00Z")
  , testCase "from jun 13 1977" $
    dt "2009-02-19T01:46:40Z" @=? fromDay (dt "1977-06-13T00:00:00Z")
  , testCase "from jul 19 1959" $
    dt "1991-03-27T01:46:40Z" @=? fromDay (dt "1959-07-19T00:00:00Z")
    -- customize this to test your birthday and find your gigasecond date:
  , testCase "from feb 8 1989" $
    dt "2020-10-18T04:21:40Z" @=? fromDay (dt "1989-02-09T02:35:00Z")
  ]
