module Puzzle2 where

import Puzzle1 (MoveCommand (Down, Forward, Up), parseInput)

solve2 :: [MoveCommand] -> Int
solve2 moveCommands = horizontalPosition * depth
  where
    (horizontalPosition, depth) =
      foldl
        applyMoveCommandWithAim
        (0, 0)
        moveCommandsWithAimValues
    moveCommandsWithAimValues =
      zip moveCommands (aimValues moveCommands)

applyMoveCommandWithAim (x1, y1) (positionChange, aim) = case positionChange of
  Forward x -> (x1 + x, y1 + (x * aim))
  _ -> (x1, y1)

aimValues :: [MoveCommand] -> [Int]
aimValues = scanl1 (+) . map aimChange
  where
    aimChange :: MoveCommand -> Int
    aimChange (Down y) = y
    aimChange (Up y) = - y
    aimChange _ = 0

main = interact $ show . solve2 . parseInput
