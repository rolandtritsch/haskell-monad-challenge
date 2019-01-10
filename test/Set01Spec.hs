module Set01Spec where

import Test.Hspec

import Set01

run :: IO ()
run = hspec $ do
  describe "fiveRands" $ do
    it "should return the correct result" $ do
      prd fiveRands `shouldBe` 8681089573064486461641871805074254223660
