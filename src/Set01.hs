module Set01 where

import MCPrelude (Seed, rand, mkSeed, toLetter)

fiveRands :: [Integer]
fiveRands = go (mkSeed 1) 5 where
  go s 1 = [fst $ rand s]
  go s n = i' : (go s' n') where
    (i', s') = rand s
    n' = n - 1

prd :: [Integer] -> Integer
prd = foldl (*) 1

threeRandsStr :: String
threeRandsStr = go (mkSeed 1) 3 where
  go s 1 = [fst $ randLetter s]
  go s n = c' : (go s' n') where
    (c', s') = randLetter s
    n' = n - 1

type Gen a = Seed -> (a, Seed)

generalA :: (a -> b) -> Gen a -> Gen b
generalA f r s = (f i', s') where
  (i', s') = r s

randLetter :: Gen Char
randLetter = generalA toLetter rand

randEven :: Gen Integer
randEven = generalA ((*) 2) rand

randOdd :: Gen Integer
randOdd = generalA (\i -> i * 2 + 1) rand

randTen :: Gen Integer
randTen = generalA ((*) 10) rand

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair rx ry s = ((x', y'), s'') where
  (x', s') = rx s
  (y', s'') = ry s'

randPair :: Gen (Char, Integer)
randPair = generalB (,) randLetter rand

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB ctor rx ry s = (ctor x' y', s'') where
  (x', s') = rx s
  (y', s'') = ry s'

repRandom :: [Gen a] -> Gen [a]
repRandom [] s = ([], s)
repRandom (rx:rxs) s = (x' : xs'', s'') where
  (x', s') = rx s
  (xs'', s'') = repRandom rxs s'

repRandom' :: [Gen a] -> Gen [a]
repRandom' rxs s = foldl apply ([], s) rxs where
  apply (xs', s') rx' = (xs'', s'') where
    (rx'', s'') = rx' s'
    xs'' = xs' ++ [rx'']

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo rx f s = f x' s' where
  (x', s') = rx s

mkGen :: a -> Gen a
mkGen = (,)

generalB' :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB' ctor rx ry = genTwo rx (\x ->
  genTwo ry (\y ->
    mkGen (ctor x y)))

repRandom'' :: [Gen a] -> Gen [a]
repRandom'' [] = mkGen []
repRandom'' (rx:rxs) = genTwo rx (\x ->
  genTwo (repRandom'' rxs) (\y ->
    mkGen (x : y)))
