module Puzzle1 where

import Data.Bits (Bits (setBit))
import Data.Function (on)
import Data.List (group, maximumBy, sort, transpose)

type BitString = [Bool]

solve1 :: [BitString] -> Int
solve1 input = bitStringToInt gama * bitStringToInt epsilon
  where
    gama = map mostCommon . transpose $ input
    epsilon = map not gama

mostCommon :: Ord a => [a] -> a
mostCommon = head . maximumBy (compare `on` length) . group . sort

bitStringToInt :: [Bool] -> Int
bitStringToInt =
  foldl reducer 0
    . zip [0 ..]
    . reverse
  where
    reducer acc (i, b) = if b then setBit acc i else acc

parse :: String -> [BitString]
parse = map (map readBit) . lines

readBit :: Char -> Bool
readBit c = case c of
  '0' -> False
  '1' -> True
  _ -> error "invalid bit value"

main = interact $ show . solve1 . parse
