module Puzzle2 where

import Control.Monad (forM_, liftM2)
import Data.Array (elems)
import Data.Array.ST
  ( newListArray,
    readArray,
    runSTArray,
    writeArray,
  )
import Data.Char (isDigit)

type Point = (Int, Int)

type Line = (Point, Point)

solve2 :: [Line] -> Int
solve2 lines =
  count (> 1)
    . elems
    $ runSTArray (drawLines lines)

drawLines lines = do
  let gridWidth = calcGridWidth lines
  let gridHeight = calcGridHeight lines
  grid <- newListArray ((0, 0), (gridWidth, gridHeight)) (repeat 0)
  forM_ lines $ \line -> do
    let indices = getLineIndices line
    forM_ indices $ \i -> do
      val <- readArray grid i
      writeArray grid i (val + 1)
  return grid

getLineIndices ((x1, y1), (x2, y2))
  | x1 == x2 = map (\y -> (x1, y)) ys
  | y1 == y2 = map (\x -> (x, y1)) xs
  | otherwise = zip xs ys
  where
    ys = if y1 < y2 then [y1 .. y2] else reverse [y2 .. y1]
    xs = if x1 < x2 then [x1 .. x2] else reverse [x2 .. x1]

calcGridWidth :: [Line] -> Int
calcGridWidth = maximum . (map (fst . fst) <++> map (fst . snd))

calcGridHeight :: [Line] -> Int
calcGridHeight = maximum . (map (snd . fst) <++> map (snd . snd))

(<++>) = liftM2 (++)

count pred = length . filter pred

parseLine :: String -> Line
parseLine inputLine = ((x1, y1), (x2, y2))
  where
    [x1, y1, x2, y2] = map read . words . map replaceNonDigit $ inputLine
    replaceNonDigit c = if isDigit c then c else ' '

parseInput = map parseLine . lines

main = interact $ show . solve2 . parseInput
