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

getLineIndices ((x1, y1), (x2, y2)) = zip xs ys
  where
    oneDimension a b = 
      if a == b then
        repeat a
      else if a < b then
        [a .. b] 
      else
        reverse [b .. a]
    xs = oneDimension x1 x2
    ys = oneDimension y1 y2

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
