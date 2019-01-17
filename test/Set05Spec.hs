module Set05Spec where

import Test.Hspec

import Prelude hiding (Maybe(..))

import MCPrelude

import Set04 (Gen(..), Maybe(..))
import Set05

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

  describe "fiveRands" $ do
    it "should return the correct result" $ do
      fiveRands `shouldBe` 8681089573064486461641871805074254223660
