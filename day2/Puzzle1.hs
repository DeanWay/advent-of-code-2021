module Puzzle1 where

type Position = (Int, Int)

type PositionChange = Position

parseCommand :: String -> PositionChange
parseCommand s = case command of
  ("forward", x) -> (x, 0)
  ("down", x) -> (0, x)
  ("up", x) -> (0, - x)
  where
    command = case words s of
      [direction, x] -> (direction, read x)

parse = map parseCommand . lines

solve1 :: [PositionChange] -> Int
solve1 = uncurry (*) . finalPosition (0, 0)

finalPosition :: Position -> [PositionChange] -> Position
finalPosition = foldl addPostions

addPostions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

main = interact $ show . solve1 . parse
