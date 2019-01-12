module Set03 where

import MCPrelude

allPairs :: [a] -> [b] -> [(a,b)]
allPairs xs ys = [(x, y) | x <- xs, y <-ys]

allPairs' :: [a] -> [b] -> [(a,b)]
allPairs' xs ys = concatMap (\x -> map (\y -> (x, y)) ys) xs

allPairs'' :: [a] -> [b] -> [(a,b)]
allPairs'' (x:[]) ys = allPair x ys
allPairs'' (x:xs) ys = allPair x ys ++ allPairs xs ys

allPair :: a -> [b] -> [(a,b)]
allPair _ [] = []
allPair x' (y':ys') = (x', y') : allPair x' ys'

data Card = Card {
  rank :: Int,
  suite :: String
  }

instance Show Card where
  show (Card r s)  = (show r) ++ s

allCards :: [Int] -> [String] -> [Card]
allCards ranks suites = map mkCard $ allPairs'' ranks suites where
  mkCard (r, s) = Card r s
