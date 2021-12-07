module Puzzle2 where

import Puzzle1
  ( applyDrawToBoard,
    bingoBoardIsWinner,
    firstWith,
    gameStep,
    readAndParseInput,
    scoreOfBoard,
  )

solve2 draws boards = scoreOfBoard lastDraw lastWinner
  where
    gameSteps = iterate gameStep (draws, boards)
    containsOnlyWinning = all bingoBoardIsWinner
    stepsWithSomeLosers = takeWhile (not . containsOnlyWinning . snd) gameSteps
    lastDraw = head . fst . last $ stepsWithSomeLosers
    lastWinnerBeforeLastDraw =
      firstWith (not . bingoBoardIsWinner)
        . snd
        . last
        $ stepsWithSomeLosers
    lastWinner = applyDrawToBoard lastDraw lastWinnerBeforeLastDraw

main = do
  (draws, boards) <- readAndParseInput
  print $ solve2 draws boards
