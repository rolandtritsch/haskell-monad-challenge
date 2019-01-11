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
headMay (a:_) = Just a

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:as) = Just as

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
maximumMay as = Just (maximum as)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay as = Just (minimum as)

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
fromJust (Just a) = a
fromJust Nothing = error "fromJust: Nothing"
