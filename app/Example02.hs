module Example02 where

import Text.Printf (printf)

import MCPrelude (
  greekDataB
  )

import Set02 (
  queryGreek,
  fromJust,
  tailMay,
  maximumMay,
  chain
  )

main :: IO ()
main = do
  let d = fromJust $ queryGreek greekDataB "chi"
  printf "%f\n" d
  let m :: Integer; m = fromJust $ chain maximumMay (tailMay ([10, 8, 9, 7]))
  printf "%d\n" m
