module Puzzle1 where

import Data.Char (isDigit)

solve1 numberOfDays = sum . map (multipliesToOnNthDay . (numberOfDays -))
  where
    fishPerDay = days [0]
    multipliesToOnNthDay n = length $ fishPerDay !! n

days = iterate dayStep

dayStep :: [Int] -> [Int]
dayStep fish = oldFish ++ newFish
  where
    oldFish = map (wrap . subtract 1) fish
    newFish = replicate (count (== 0) fish) 8
    wrap x = if x < 0 then 6 else x

parseInput :: String -> [Int]
parseInput = map read . words . map replaceNonDigit
  where
    replaceNonDigit c = if isDigit c then c else ' '

count pred = length . filter pred

main = interact $ show . solve1 80 . parseInput
