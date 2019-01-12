module Example03 where

import Text.Printf (printf)

import MCPrelude

import Set03

main :: IO ()
main = do
  printf "%s\n" (show $ allPairs'' [1, 2] [3, 4])
