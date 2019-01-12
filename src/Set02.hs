module Set02 where

import Prelude hiding (Maybe, Nothing, Just)

import MCPrelude

data Maybe a = Just a | Nothing

instance Show a => Show (Maybe a) where
  show (Just a) = "Just " ++ (show a)
  show Nothing = "Nothing"

instance Eq a => Eq (Maybe a) where
  (==) (Just a) (Just b) = a == b
  (==) Nothing Nothing = True
  (==) _ _ = False

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay key ((key', value'):rest)
  | key == key' = Just value'
  | otherwise = lookupMay key rest

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay _ 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just (maximum xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just (minimum xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek database key = result where
  result
    | isNothing max' = Nothing
    | isNothing head' = Nothing
    | otherwise = divMay (fromIntegral $ fromJust max') (fromIntegral $ fromJust head')
  max'
    | isNothing tail' = Nothing
    | otherwise = maximumMay (fromJust tail')
  tail'
    | isNothing values' = Nothing
    | otherwise = tailMay (fromJust values')
  head'
    | isNothing values' = Nothing
    | otherwise = headMay (fromJust values')
  values' = lookupMay key database

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

chain :: (a -> Maybe b) -> Maybe a -> Maybe b
chain _ Nothing = Nothing
chain f (Just x) = f x

link :: Maybe a -> (a -> Maybe b) -> Maybe b
link = flip chain

mkPair :: Maybe a -> Maybe a -> Maybe (a, a)
mkPair (Just x) (Just y) = Just (x, y)
mkPair _ _ = Nothing

queryGreek' :: GreekData -> String -> Maybe Double
queryGreek' database key = result where
  result = chain (\(x, y) -> divMay (fromIntegral x) (fromIntegral y)) (mkPair max' head')
  max' = chain maximumMay tail'
  tail' = chain tailMay values'
  head' = chain headMay values'
  values' = lookupMay key database

queryGreek'' :: GreekData -> String -> Maybe Double
queryGreek'' database key =
  link (lookupMay key database) (\values' ->
    link (link (tailMay values') maximumMay) (\max' ->
      link (headMay values') (\head' ->
        divMay (fromIntegral max') (fromIntegral head'))))

salaries :: [(String, Integer)]
salaries = [
  ("alice", 105000),
  ("bob", 90000),
  ("carol", 85000)
  ]

mkMaybe :: a -> Maybe a
mkMaybe x = Just x

plusMay :: Num a => Maybe a -> Maybe a -> Maybe a
plusMay (Just x) (Just y) = mkMaybe (x + y)
plusMay _ _ = Nothing

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries database key key' = plusMay x y where
  x = lookupMay key database
  y = lookupMay key' database

addSalaries' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries' database key key' = link (lookupMay key database) (\x ->
  link (lookupMay key' database) (\y ->
    mkMaybe (x + y)))

yLink :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
yLink f (Just x) (Just y) = mkMaybe (f x y)
yLink _ _ _ = Nothing

addSalaries'' :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries'' database key key' = yLink (+) (lookupMay key database) (lookupMay key' database)

tailProd :: Num a => [a] -> Maybe a
tailProd [] = Nothing
tailProd (_:[]) = Just 1
tailProd (_:rest) = Just (product rest)

tailSum :: Num a => [a] -> Maybe a
tailSum [] = Nothing
tailSum (_:[]) = Just 0
tailSum (_:rest) = Just (sum rest)

transMaybe :: a -> ([a] -> a) -> [a] -> Maybe a
transMaybe _ _ [] = Nothing
transMaybe x _ (_:[]) = Just x
transMaybe x f (_:rest) = Just (f rest)

tailProd' :: Num a => [a] -> Maybe a
tailProd' xs = transMaybe 1 product xs

tailSum' :: Num a => [a] -> Maybe a
tailSum' xs = transMaybe 0 sum xs

transMaybe' :: (a -> b) -> Maybe a -> Maybe b
transMaybe' _ Nothing = Nothing
transMaybe' f (Just x) = mkMaybe (f x)

tailProd'' :: Num a => [a] -> Maybe a
tailProd'' xs = transMaybe' product (tailMay xs)

tailSum'' :: Num a => [a] -> Maybe a
tailSum'' xs = transMaybe' sum (tailMay xs)

tailMin :: Ord a => [a] -> Maybe a
tailMin xs = transMaybe' minimum (tailMay xs)

tailMax :: Ord a => [a] -> Maybe a
tailMax xs = transMaybe' maximum (tailMay xs)
