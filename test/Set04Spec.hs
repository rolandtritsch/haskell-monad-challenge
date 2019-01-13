module Set04Spec where

import Test.Hspec

import MCPrelude

import Set04

run :: IO ()
run = hspec $ do
  describe "reverse" $ do
    it "should return the correct result(s)" $ do
      reverse "dnalor" `shouldBe` "roland"
