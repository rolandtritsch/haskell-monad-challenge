--{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set05 where

import MCPrelude

import Data.List (replicate)

import Set04 (
  Gen(..),
  Monad(..),
  (>>=),
  Maybe(..),
  lookupMay,
  tailMay,
  headMay,
  minimumMay,
  maximumMay,
  divMay,
  sequence,
  evalGen
  )

randInteger :: Gen Integer
randInteger = do
  Gen rand

randLetter :: Gen Char
randLetter = do
  i <- randInteger
  let c = toLetter i
  return c

randEven :: Gen Integer
randEven = do
  i <- randInteger
  return (i * 2)

randOdd :: Gen Integer
randOdd = do
  i <- randEven
  return (i + 1)

randTen :: Gen Integer
randTen = do
  i <- randInteger
  return (i * 10)

randPair :: Gen (Char, Integer)
randPair = do
  c <- randLetter
  i <- randInteger
  return (c, i)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek database key = do
  values <- lookupMay key database
  tailValues <- tailMay values
  maxValue <- maximumMay tailValues
  headValue <- headMay values
  divMay (fromIntegral maxValue) (fromIntegral headValue)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries database key key' = do
  value <- lookupMay key database
  value' <- lookupMay key' database
  return (value + value')

tailProd :: Num a => [a] -> Maybe a
tailProd xs = do
  tailXs <- tailMay xs
  return (product tailXs)

tailSum :: Num a => [a] -> Maybe a
tailSum xs = do
  tailXs <- tailMay xs
  return (sum tailXs)

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = do
  tailXs <- tailMay xs
  minimumMay tailXs

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = do
  tailXs <- tailMay xs
  maximumMay tailXs

fiveRands :: Integer
fiveRands = product $ evalGen fiveRands' (mkSeed 1) where
  fiveRands' = sequence (replicate 5 (Gen rand))
