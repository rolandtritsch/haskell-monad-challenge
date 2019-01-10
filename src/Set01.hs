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

randLetter :: Seed -> (Char, Seed)
randLetter s = (c', s') where
  (i', s') = rand s
  c' = toLetter i'

threeRandsStr :: String
threeRandsStr = go (mkSeed 1) 3 where
  go s 1 = [fst $ randLetter s]
  go s n = c' : (go s' n') where
    (c', s') = randLetter s
    n' = n - 1
