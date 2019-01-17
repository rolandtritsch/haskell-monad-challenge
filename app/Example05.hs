module Example05 where

import Text.Printf (printf)

import MCPrelude

import Set04 (Gen(..), fromJust)
import Set05

main :: IO ()
main = do
  let (e, _) = (runGen randEven) (mkSeed 1)
  let (o, _) = (runGen randOdd) (mkSeed 1)
  let (t, _) = (runGen randTen) (mkSeed 1)
  printf "%d\n" (e * o * t)
  let (p, _) = (runGen randPair) (mkSeed 1)
  printf "%s\n" (show p)
  let d = fromJust $ queryGreek greekDataB "chi"
  printf "%f\n" d
  let s = fromJust $ addSalaries salaries "alice" "carol"
  printf "%d\n" s
  let tp :: Integer; tp = fromJust $ tailProd [1, 2, 3]
  printf "%d\n" tp
  let ts :: Integer; ts = fromJust $ tailSum [1, 2, 3]
  printf "%d\n" ts
  let tmin :: Integer; tmin = fromJust $ tailMin [0, 2, 3, 1]
  printf "%d\n" tmin
  let tmax :: Integer; tmax = fromJust $ tailMax [0, 2, 3, 1]
  printf "%d\n" tmax
