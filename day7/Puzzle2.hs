module Puzzle2 where

import Puzzle1 (parseInput)

solve2 :: [Int] -> Int
solve2 crabs = minimum costsAtAllCenters
  where
    distance x y = abs (x - y)
    cost center crab =
      let n = distance center crab
       in (n * (n + 1)) `div` 2
    min' = minimum crabs
    max' = maximum crabs
    costsAtAllCenters =
      map
        (\center -> sum $ map (cost center) crabs)
        [min' .. max']

main = interact $ show . solve2 . parseInput
