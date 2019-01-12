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
repRandom (gx:gxs) s = (x' : xs'', s'') where
  (x', s') = gx s
  (xs'', s'') = repRandom gxs s'

repRandom' :: [Gen a] -> Gen [a]
repRandom' gxs s = foldl apply ([], s) gxs where
  apply (xs', s') gx' = (xs'', s'') where
    (gx'', s'') = gx' s'
    xs'' = xs' ++ [gx'']

genTwo :: Gen a -> (a -> Gen b) -> Gen b
genTwo gx f s = f x' s' where
  (x', s') = gx s

mkGen :: a -> Gen a
mkGen = (,)
