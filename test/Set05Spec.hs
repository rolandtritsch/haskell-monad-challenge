module Set05Spec where

import Test.Hspec

import MCPrelude

import Set05

run :: IO ()
run = hspec $ do
  describe "reverse" $ do
    it "should return the correct result" $ do
      reverse "dnalor" `shouldBe` "roland"
