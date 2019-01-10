module Set01Spec where

import Test.Hspec

import MCPrelude (mkSeed)

import Set01

run :: IO ()
run = hspec $ do
  describe "fiveRands" $ do
    it "should return the correct result" $ do
      prd fiveRands `shouldBe` 8681089573064486461641871805074254223660

  describe "threeRandsStr" $ do
    it "should return the correct result" $ do
      threeRandsStr `shouldBe` "lrf"

  describe "randEvenOddTen" $ do
    it "should return the correct result" $ do
      let (e, _) = randEven (mkSeed 1)
      let (o, _) = randOdd (mkSeed 1)
      let (t, _) = randTen (mkSeed 1)
      e * o * t `shouldBe` 189908109902700

  describe "randPair" $ do
    it "should return the correct result" $ do
      let (p, _) = randPair (mkSeed 1)
      p `shouldBe` ('l', 282475249)
