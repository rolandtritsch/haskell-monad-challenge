module Example03 where

import Text.Printf (printf)

import MCPrelude

import Set03

main :: IO ()
main = do
  printf "%s\n" (show $ allPairs''' [1, 2] [3, 4])
  printf "%s\n" (show $ allCards' cardRanks cardSuits)
  printf "%s\n" (show $ allCombs' (,) [1,2] [3,4])
  printf "%s\n" (show $ allCombs3' (,,) [1,2] [3,4] [5,6])
