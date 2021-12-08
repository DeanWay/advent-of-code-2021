module Puzzle1 where

import Data.Char (isDigit)
import Data.List (sort)

median xs
  | even len = sorted !! mid
  | odd len = (sorted !! (mid - 1) + sorted !! mid) / 2
  where
    sorted = sort xs
    len = length sorted
    mid = len `div` 2

solve1 :: [Int] -> Int
solve1 crabs = sum . map (abs . subtract median') $ crabs
  where
    median' = round $ median $ map fromIntegral crabs

parseInput :: String -> [Int]
parseInput = map read . words . map replaceNonDigit
  where
    replaceNonDigit c = if isDigit c then c else ' '

main = interact $ show . solve1 . parseInput
