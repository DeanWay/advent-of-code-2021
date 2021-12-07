module Puzzle2 where

import Data.List (tails)
import Puzzle1 (solve1)

solve2 = solve1 . map sum . slidingWindow 3

slidingWindow n = filter ((== n) . length) . map (take n) . tails
