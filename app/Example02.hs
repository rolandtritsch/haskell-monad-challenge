module Example02 where

import Text.Printf (printf)

import MCPrelude (
  greekDataB
  )

import Set02

main :: IO ()
main = do
  let m :: Integer; m = fromJust $ chain maximumMay (tailMay ([10, 8, 9, 7]))
  printf "%d\n" m
  let d = fromJust $ queryGreek'' greekDataB "chi"
  printf "%f\n" d
  let s = fromJust $ addSalaries'' salaries "alice" "carol"
  printf "%d\n" s
  let tp :: Integer; tp = fromJust $ tailProd'' [1, 2, 3]
  printf "%d\n" tp
  let ts :: Integer; ts = fromJust $ tailSum'' [1, 2, 3]
  printf "%d\n" ts
  let tmin :: Integer; tmin = fromJust $ tailMin'' [0, 2, 3, 1]
  printf "%d\n" tmin
  let tmax :: Integer; tmax = fromJust $ tailMax'' [0, 2, 3, 1]
  printf "%d\n" tmax
