module Set04Spec where

import Test.Hspec

import Prelude hiding (Maybe(..))

import MCPrelude

import Set04

run :: IO ()
run = hspec $ do
  describe "randEvenOddTen" $ do
    it "should return the correct result" $ do
      let (e, _) = (runGen randEven) (mkSeed 1)
      let (o, _) = (runGen randOdd) (mkSeed 1)
      let (t, _) = (runGen randTen) (mkSeed 1)
      e * o * t `shouldBe` 189908109902700

  describe "randPair" $ do
    it "should return the correct result" $ do
      let (p, _) = (runGen randPair) (mkSeed 1)
      p `shouldBe` ('l', 282475249)

  describe "repRandom" $ do
    it "should return the correct result" $ do
      let (s, _) = runGen (repRandom (replicate 3 randLetter)) (mkSeed 1)
      s `shouldBe` "lrf"

  describe "queryGreek" $ do
    it "should return the correct result(s)" $ do
      queryGreek greekDataA "alpha" `shouldBe` Just 2.0
      queryGreek greekDataA "beta" `shouldBe` Nothing
      queryGreek greekDataA "gamma" `shouldBe` Just 3.3333333333333335
      queryGreek greekDataA "delta" `shouldBe` Nothing
      queryGreek greekDataA "zeta" `shouldBe` Nothing

      queryGreek greekDataB "rho" `shouldBe` Nothing
      queryGreek greekDataB "phi" `shouldBe` Just 0.24528301886792453
      queryGreek greekDataB "chi" `shouldBe` Just 9.095238095238095
      queryGreek greekDataB "psi" `shouldBe` Nothing
      queryGreek greekDataB "omega" `shouldBe` Just 24.0

  describe "addSalaries" $ do
    it "should return the correct result(s)" $ do
      addSalaries salaries "alice" "carol" `shouldBe` Just 190000

  describe "tailProd" $ do
    it "should return the correct result(s)" $ do
      tailProd [1, 2, 3] `shouldBe` Just 6

  describe "tailSum" $ do
    it "should return the correct result(s)" $ do
      tailSum [1, 2, 3] `shouldBe` Just 5

  describe "tailMin" $ do
    it "should return the correct result(s)" $ do
      tailMin [0, 2, 3, 1] `shouldBe` Just 1

  describe "tailMax" $ do
    it "should return the correct result(s)" $ do
      tailMax [0, 2, 3, 1] `shouldBe` Just 3

  describe "allPairs" $ do
    it "should return the correct result(s)" $ do
      allPairs [1,2] [3,4] `shouldBe` [(1,3),(1,4),(2,3),(2,4)]
      allPairs [1..3] [6..8] `shouldBe` [(1,6),(1,7),(1,8),(2,6),(2,7),(2,8),(3,6),(3,7),(3,8)]
      allPairs cardRanks cardSuits `shouldBe` [(2,"H"),(2,"D"),(2,"C"),(2,"S"),(3,"H"),(3,"D"),(3,"C"),(3,"S"),(4,"H"),(4,"D"),(4,"C"),(4,"S"),(5,"H"),(5,"D"),(5,"C"),(5,"S")]

  describe "allCards" $ do
    it "should return the correct result(s)" $ do
      show (allCards cardRanks cardSuits) `shouldBe` "[2H,2D,2C,2S,3H,3D,3C,3S,4H,4D,4C,4S,5H,5D,5C,5S]"
