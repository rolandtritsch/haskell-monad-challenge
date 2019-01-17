module Set04 where

import Prelude hiding (Monad, Maybe(..), return, sequence)

import MCPrelude

-- Define a/the monad type class

class Monad m where
  bind :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- Define the monads

data Maybe a = Just a | Nothing deriving (Show, Eq)

instance Monad Maybe where
  bind Nothing _ = Nothing
  bind (Just x) f = f x
  return = Just

instance Monad [] where
  bind [] _ = []
  bind (x:xs) f = f x ++ bind xs f
  return x = [x]

newtype Gen a = Gen {
  runGen :: Seed -> (a, Seed)
  }

instance Monad Gen where
  return x = Gen mkGen where
    mkGen s = (x, s)
  bind gx f = Gen bindGen where
    bindGen s = runGen (f a) s' where
      (a, s') = runGen gx s

evalGen :: Gen a -> Seed -> a
evalGen gx s = x where
  (x, _) = runGen gx s

-- Define the standard abstractions

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (mx:mxs) = bind mx bindMxs where
  bindMxs x = bind (sequence mxs) bindMxs' where
    bindMxs' x' = return (x : x')

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = bind mx bindMx where
  bindMx x = return (f x)

liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = bind mx bindMx where
  bindMx x = bind my bindMy where
    bindMy y = return (f x y)

liftM3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftM3 f mx my mz = bind mx bindMx where
  bindMx x = bind my bindMy where
    bindMy y = bind mz bindMz where
      bindMz z = return (f x y z)

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = flip bind

(>>=) :: Monad m => m a -> (a -> m b) -> m b
(>>=) = bind

join :: Monad m => m (m a) -> m a
join mmx = bind mmx id

ap :: Monad m => m (a -> b) -> m a -> m b
ap mf mx = bind mx bindMx where
  bindMx x = bind mf bindMf where
    bindMf f = return (f x)

-- Rewrite Set01

randInteger :: Gen Integer
randInteger = Gen rand

randLetter :: Gen Char
randLetter = bind randInteger bindLetter where
  bindLetter = return . toLetter

randEven :: Gen Integer
randEven = bind randInteger bindEven where
  bindEven i = return (i * 2)

randOdd :: Gen Integer
randOdd = bind randEven bindOdd where
  bindOdd i = return (i + 1)

randTen :: Gen Integer
randTen = bind randInteger bindTen where
  bindTen i = return (i * 10)

randPair :: Gen (Char, Integer)
randPair = bind randLetter bindLetter where
  bindLetter c = bind randInteger bindInteger where
    bindInteger i = return (c, i)

repRandom :: [Gen a] -> Gen [a]
repRandom = sequence

-- Rewrite Set02

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"

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
queryGreek database key = bind (lookupMay key database) values where
  values vs = bind (bind (tailMay vs) maximumMay) max where
    max m = bind (headMay vs) head where
      head h = divMay (fromIntegral m) (fromIntegral h)

addSalaries :: [(String, Integer)] -> String -> String -> Maybe Integer
addSalaries database key key' = liftM2 (+) (lookupMay key database) (lookupMay key' database)

tailProd :: Num a => [a] -> Maybe a
tailProd = liftM product . tailMay

tailSum :: Num a => [a] -> Maybe a
tailSum = liftM sum . tailMay

tailMin :: Ord a => [a] -> Maybe a
tailMin = join . liftM minimumMay . tailMay

tailMax :: Ord a => [a] -> Maybe a
tailMax = join . liftM maximumMay . tailMay

-- Rewrite Set03

allPairs :: [a] -> [b] -> [(a,b)]
allPairs = liftM2 (,)

data Card = Card {
  rank :: Int,
  suite :: String
  }

instance Show Card where
  show (Card r s)  = (show r) ++ s

allCards :: [Int] -> [String] -> [Card]
allCards = liftM2 Card
