module Puzzle1 where

import Puzzle2 (parseInput, solve2)

solve1 = solve2 . onlyVerticalAndHorizontal

isVertical ((_, y1), (_, y2)) = y1 == y2

isHorizontal ((x1, _), (x2, _)) = x1 == x2

onlyVerticalAndHorizontal = filter (\x -> isVertical x || isHorizontal x)

main = interact $ show . solve1 . parseInput
