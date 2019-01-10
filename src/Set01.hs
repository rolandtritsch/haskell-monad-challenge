module Set01 where

import MCPrelude (rand, mkSeed)

fiveRands :: [Integer]
fiveRands = go (mkSeed 1) 5 where
  go s 1 = [fst $ rand s]
  go s n = i' : (go s' n') where
    (i', s') = rand s
    n' = n - 1

prd :: [Integer] -> Integer
prd = foldl (*) 1
