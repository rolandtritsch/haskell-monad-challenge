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
      allPairs'' cardRanks cardSuits `shouldBe` [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]

  describe "show" $ do
    it "should return the correct result(s)" $ do
      show (Card 2 "H") `shouldBe` "2H"

  describe "allCards" $ do
    it "should return the correct result(s)" $ do
      show (allCards cardRanks cardSuits) `shouldBe` "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
