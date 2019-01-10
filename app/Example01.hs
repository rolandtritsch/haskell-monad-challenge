module Example01 where

import Text.Printf (printf)

import MCPrelude (mkSeed)

import Set01 (
  fiveRands,
  prd,
  threeRandsStr,
  randEven,
  randOdd,
  randTen,
  randPair
  )

main :: IO ()
main = do
  printf "%d\n" (prd fiveRands)
  printf "%s\n" threeRandsStr
  let (e, _) = randEven (mkSeed 1)
  let (o, _) = randOdd (mkSeed 1)
  let (t, _) = randTen (mkSeed 1)
  printf "%d\n" (e * o * t)
  let (p, _) = randPair (mkSeed 1)
  printf "%s\n" (show p)
