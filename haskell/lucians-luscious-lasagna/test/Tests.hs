import LuciansLusciousLasagna (elapsedTimeInMinutes, expectedMinutesInOven, preparationTimeInMinutes)
import Test.Hspec (hspec, it, shouldBe)

main :: IO ()
main = hspec $ do
  it "expectedMinutesInOven" $ do
    expectedMinutesInOven `shouldBe` 40

  it "preparationTimeInMinutes" $
    preparationTimeInMinutes 5 `shouldBe` 10

  it "elapsedTimeInMinutes" $ do
    elapsedTimeInMinutes 3 20 `shouldBe` 26
