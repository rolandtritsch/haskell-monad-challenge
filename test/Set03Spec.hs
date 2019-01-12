module Set03Spec where

import Test.Hspec

import MCPrelude

import Set03

run :: IO ()
run = hspec $ do
  describe "allPairs" $ do
    it "should return the correct result(s)" $ do
      allPairs [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      allPairs' [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      allPairs'' [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      allPairs [1..3] [6..8] `shouldBe` [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
      allPairs' [1..3] [6..8] `shouldBe` [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
      allPairs'' [1..3] [6..8] `shouldBe` [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
