module Puzzle1 where

type Position = (Int, Int)

data MoveCommand = Forward Int | Up Int | Down Int

solve1 :: [MoveCommand] -> Int
solve1 moveCommands = horizontalPosition * depth
  where
    (horizontalPosition, depth) = applyMoveCommands (0, 0) moveCommands

applyMoveCommands :: Position -> [MoveCommand] -> Position
applyMoveCommands = foldl applyMoveCommand

applyMoveCommand :: Position -> MoveCommand -> Position
applyMoveCommand (x1, y1) change = case change of
  Forward x -> (x1 + x, y1)
  Down y -> (x1, y1 + y)
  Up y -> (x1, y1 - y)

parseMoveCommand :: String -> MoveCommand
parseMoveCommand s = case command of
  ("forward", x) -> Forward x
  ("down", x) -> Down x
  ("up", x) -> Up x
  where
    command = case words s of
      [direction, x] -> (direction, read x)

parseInput = map parseMoveCommand . lines

main = interact $ show . solve1 . parseInput
