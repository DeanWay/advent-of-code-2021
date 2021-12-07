module Puzzle1 where

import Data.Function (on)
import Data.List (find, groupBy, transpose)
import Data.Maybe (isNothing)

type BingoBoard = [[BingoCell]]

data BingoCell = BingoCell
  { cellValue :: Int,
    cellIsMarked :: Bool
  }

instance Show BingoCell where
  show cell = if cellIsMarked cell then "*" else show $ cellValue cell

bingoBoardIsWinner :: BingoBoard -> Bool
bingoBoardIsWinner board = rowCompleted || columnCompleted
  where
    rowCompleted :: Bool
    rowCompleted = any and boardCheckedValues
    columnCompleted = any and (transpose boardCheckedValues)
    boardCheckedValues = map (map cellIsMarked) board

parseBingoBoard :: [String] -> BingoBoard
parseBingoBoard = map (map (makeCell . read) . words)
  where
    makeCell x = BingoCell {cellValue = x, cellIsMarked = False}

solve1 draws boards = scoreOfBoard lastDraw winner
  where
    gameSteps = iterate gameStep (draws, boards)
    containsOnlyLosing = isNothing . find bingoBoardIsWinner
    stepsWithWinners = dropWhile (containsOnlyLosing . snd) gameSteps
    stepsWithNoWinners = takeWhile (containsOnlyLosing . snd) gameSteps
    lastDraw = head . fst . last $ stepsWithNoWinners
    winner = firstWith bingoBoardIsWinner . snd . head $ stepsWithWinners

gameStep :: ([Int], [BingoBoard]) -> ([Int], [BingoBoard])
gameStep (draw : remainingDraws, boards) =
  (remainingDraws, boardsWithDrawApplied)
  where
    boardsWithDrawApplied = map (applyDrawToBoard draw) boards

applyDrawToBoard :: Int -> BingoBoard -> BingoBoard
applyDrawToBoard draw = map (map $ applyDrawToCell draw)

applyDrawToCell :: Int -> BingoCell -> BingoCell
applyDrawToCell draw cell =
  if cellValue cell == draw
    then cell {cellIsMarked = True}
    else cell

scoreOfBoard :: Int -> BingoBoard -> Int
scoreOfBoard lastDraw board = sumUnmarked * lastDraw
  where
    sumUnmarked = sum . map cellScore . concat $ board
    cellScore cell = if cellIsMarked cell then 0 else cellValue cell

main = do
  (draws, boards) <- readAndParseInput
  print $ solve1 draws boards

readAndParseInput :: IO ([Int], [BingoBoard])
readAndParseInput = do
  draws <- parseDraws <$> getLine
  boards <- parseBoards <$> getContents
  return (draws, boards)

parseDraws :: String -> [Int]
parseDraws = map read . words . map (\c -> if c == ',' then ' ' else c)

parseBoards =
  map parseBingoBoard
    . filter ((> 1) . length)
    . groupBy ((==) `on` length)
    . lines

firstWith pred = head . filter pred
