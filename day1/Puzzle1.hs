module Puzzle1 where

solve1 :: [Int] -> Int
solve1 = sum . map fromEnum . mapAdjacent (<)

mapAdjacent f xs = zipWith f xs (tail xs)

parse :: String -> [Int]
parse = map read . lines

main = interact $ show . solve1 . parse
