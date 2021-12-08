module Puzzle2 where

import Data.Char (isDigit)
import Data.Function (on)
import Data.List (group, sort, sortBy)
import Data.Map (Map, fromList, toList, union)
import Puzzle1 (parseInput)

valueCounts =
  fromList
    . map (\items -> (head items, toInteger $ length items))
    . group
    . sort

radix size =
  map snd
    . sortBy (compare `on` fst)
    . toList
    . (`union` zeros)
    . valueCounts
  where
    zeros = fromList $ zip [0 .. size - 1] (repeat 0)

solve2 onDay fishes = sum (daySteps !! onDay)
  where
    daySteps = iterate dayStep initialState
    initialState = radix 9 fishes

dayStep (numZeros : rest) =
  take 6 rest
    ++ [rest !! 6 + numZeros]
    ++ [rest !! 7]
    ++ [numZeros]

main = interact $ show . solve2 256 . parseInput
