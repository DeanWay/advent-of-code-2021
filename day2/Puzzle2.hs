module Puzzle2 where

import Puzzle1 (PositionChange, parse)

solve2 :: [PositionChange] -> Int
solve2 positionChanges =
  uncurry (*) $ foldl addPostionChangeWithAim (0, 0) changesWithAimValues
  where
    changesWithAimValues = zip positionChanges (aims positionChanges)

addPostionChangeWithAim (x1, y1) (positionChange, aim) = case positionChange of
  (0, _) -> (x1, y1)
  (x2, 0) -> (x1 + x2, y1 + (x2 * aim))

aims :: [PositionChange] -> [Int]
aims = scanl1 (+) . map aimChange
  where
    aimChange :: PositionChange -> Int
    aimChange (0, y) = y
    aimChange _ = 0
