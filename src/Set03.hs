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

allCombs :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs mkComb (x:[]) ys = allComb mkComb x ys
allCombs mkComb (x:xs) ys = allComb mkComb x ys ++ allCombs mkComb xs ys

allComb :: (a -> b -> c) -> a -> [b] -> [c]
allComb _ _ [] = []
allComb mkComb x (y:ys) = mkComb x y : allComb mkComb x ys

allPairs''' :: [a] -> [b] -> [(a,b)]
allPairs''' xs ys = allCombs (,) xs ys

allCards' :: [Int] -> [String] -> [Card]
allCards' ranks suites = allCombs mkCard ranks suites where
  mkCard r s = Card r s

allCombs3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3 mkComb (x:[]) ys zs = allComb3 mkComb x ys zs
allCombs3 mkComb (x:xs) ys zs = allComb3 mkComb x ys zs ++ allCombs3 mkComb xs ys zs

allComb3 :: (a -> b -> c -> d) -> a -> [b] -> [c] -> [d]
allComb3 mkComb x (y:[]) zs = allComb3' mkComb x y zs
allComb3 mkComb x (y:ys) zs = allComb3' mkComb x y zs ++ allComb3 mkComb x ys zs

allComb3' :: (a -> b -> c -> d) -> a -> b -> [c] -> [d]
allComb3' _ _ _ [] = []
allComb3' mkComb x y (z:zs) = mkComb x y z : allComb3' mkComb x y zs

combStep :: [a -> b] -> [a] -> [b]
combStep fcombs xs = concatMap (\fcomb -> map fcomb xs) fcombs

allCombs' :: (a -> b -> c) -> [a] -> [b] -> [c]
allCombs' mkComb = combStep . combStep [mkComb]

allCombs3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
allCombs3' mkComb = combStep . combStep [mkComb] . combStep [mkComb]
