module Puzzle1 where

solve1 :: [Int] -> Int
solve1 = sum . map fromEnum . mapAdjacent (<)

mapAdjacent f xs = zipWith f xs (tail xs)
