module Set02Spec where

import Test.Hspec

import Prelude hiding (Maybe(..))

import MCPrelude (
  greekDataA,
  greekDataB
  )

import Set02

run :: IO ()
run = hspec $ do
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

  describe "queryGreek'" $ do
    it "should return the correct result(s)" $ do
      queryGreek' greekDataA "alpha" `shouldBe` Just 2.0
      queryGreek' greekDataA "beta" `shouldBe` Nothing
      queryGreek' greekDataA "gamma" `shouldBe` Just 3.3333333333333335
      queryGreek' greekDataA "delta" `shouldBe` Nothing
      queryGreek' greekDataA "zeta" `shouldBe` Nothing

      queryGreek' greekDataB "rho" `shouldBe` Nothing
      queryGreek' greekDataB "phi" `shouldBe` Just 0.24528301886792453
      queryGreek' greekDataB "chi" `shouldBe` Just 9.095238095238095
      queryGreek' greekDataB "psi" `shouldBe` Nothing
      queryGreek' greekDataB "omega" `shouldBe` Just 24.0

  describe "queryGreek''" $ do
    it "should return the correct result(s)" $ do
      queryGreek'' greekDataA "alpha" `shouldBe` Just 2.0
      queryGreek'' greekDataA "beta" `shouldBe` Nothing
      queryGreek'' greekDataA "gamma" `shouldBe` Just 3.3333333333333335
      queryGreek'' greekDataA "delta" `shouldBe` Nothing
      queryGreek'' greekDataA "zeta" `shouldBe` Nothing

      queryGreek'' greekDataB "rho" `shouldBe` Nothing
      queryGreek'' greekDataB "phi" `shouldBe` Just 0.24528301886792453
      queryGreek'' greekDataB "chi" `shouldBe` Just 9.095238095238095
      queryGreek'' greekDataB "psi" `shouldBe` Nothing
      queryGreek'' greekDataB "omega" `shouldBe` Just 24.0

  describe "chain" $ do
    it "should return the correct result(s)" $ do
      chain maximumMay (tailMay [10, 8, 9, 7]) `shouldBe` Just 9

  describe "link" $ do
    it "should return the correct result(s)" $ do
      link (tailMay [10, 8, 9, 7]) maximumMay `shouldBe` Just 9

  describe "addSalaries" $ do
    it "should return the correct result(s)" $ do
      addSalaries salaries "alice" "carol" `shouldBe` Just 190000
      addSalaries' salaries "alice" "carol" `shouldBe` Just 190000
      addSalaries'' salaries "alice" "carol" `shouldBe` Just 190000

  describe "tailProd" $ do
    it "should return the correct result(s)" $ do
      tailProd [1, 2, 3] `shouldBe` Just 6
      tailProd' [1, 2, 3] `shouldBe` Just 6
      tailProd'' [1, 2, 3] `shouldBe` Just 6

  describe "tailSum" $ do
    it "should return the correct result(s)" $ do
      tailSum [1, 2, 3] `shouldBe` Just 5
      tailSum' [1, 2, 3] `shouldBe` Just 5
      tailSum'' [1, 2, 3] `shouldBe` Just 5

  describe "tailMin" $ do
    it "should return the correct result(s)" $ do
      tailMin [0, 2, 3, 1] `shouldBe` Just 1
      tailMin' [0, 2, 3, 1] `shouldBe` Just(Just 1)
      tailMin'' [0, 2, 3, 1] `shouldBe` Just 1

  describe "tailMax" $ do
    it "should return the correct result(s)" $ do
      tailMax [0, 2, 3, 1] `shouldBe` Just 3
      tailMax' [0, 2, 3, 1] `shouldBe` Just(Just 3)
      tailMax'' [0, 2, 3, 1] `shouldBe` Just 3
