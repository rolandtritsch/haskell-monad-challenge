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

randPair :: Gen (Char, Integer)
randPair s = ((c', i'), s'') where
  (c', s') = randLetter s
  (i', s'') = rand s'
