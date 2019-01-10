module Example01 where

import Set01 (fiveRands, prd)
import Text.Printf (printf)

main :: IO ()
main = do
  printf "%d\n" (prd fiveRands)
