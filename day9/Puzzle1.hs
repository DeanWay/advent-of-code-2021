module Puzzle1 where

import Data.Array
  ( Array,
    Ix (inRange),
    bounds,
    indices,
    listArray,
    (!),
  )
import Data.Char (digitToInt)

solve1 arr = sum . map (toRiskValue . (arr !)) . lowPoints $ arr
  where
    toRiskValue = (+ 1)

lowPoints arr =
  filter isLessThanNeighbours
    . indices
    $ arr
  where
    isLessThanNeighbours idx =
      all (\adj -> (arr ! idx) < (arr ! adj)) (adjacent idx arr)

adjacent (x, y) arr =
  filter
    (inRange (bounds arr))
    [ (x - 1, y),
      (x, y - 1),
      (x + 1, y),
      (x, y + 1)
    ]

parseInput :: String -> Array (Int, Int) Int
parseInput = twoDListsToArray . parseLists
  where
    parseLists = map (map digitToInt) . lines

twoDListsToArray :: [[a]] -> Array (Int, Int) a
twoDListsToArray rows =
  listArray
    ((0, 0), (height, width))
    (concat rows)
  where
    height = length rows - 1
    width = length (head rows) - 1

main = interact $ show . solve1 . parseInput
