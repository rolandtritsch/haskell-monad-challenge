module Set02Spec where

import Test.Hspec

import Prelude hiding (Maybe(..))

import MCPrelude (
  greekDataA,
  greekDataB
  )

import Set02 (
  Maybe(..),
  queryGreek,
  queryGreek',
  queryGreek'',
  tailMay,
  maximumMay,
  chain,
  link
  )

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
