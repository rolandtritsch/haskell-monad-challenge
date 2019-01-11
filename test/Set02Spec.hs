module Set02Spec where

import Test.Hspec

import MCPrelude

import Set02

run :: IO ()
run = hspec $ do
  describe "hello" $ do
    it "should return the correct result" $ do
      reverse "dnalor" `shouldBe` "roland"
