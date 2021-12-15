module Puzzle2 where

import Data.Array (assocs, (!))
import Data.Graph (dfs, graphFromEdges)
import Data.List (sortBy)
import Data.Maybe (fromJust)
import Data.Tree (flatten)
import Puzzle1 (adjacent, lowPoints, parseInput)

solve2 arr = product . take 3 . sortDescending . map length $ basins
  where
    basins =
      map (map nodeValueFromVertex . flatten)
        . dfs graph
        . map (fromJust . vertexFromKey)
        $ lowPoints arr
    (graph, nodeFromVertex, vertexFromKey) = graphFromEdges allEdges
    nodeValueFromVertex = (\(nodeValue, _, _) -> nodeValue) . nodeFromVertex
    allEdges = map edgesFromIndex $ assocs arr
    edgesFromIndex (idx, val) =
      ( val,
        idx,
        filter ((/= 9) . (arr !)) (adjacent idx arr)
      )

sortDescending = sortBy (flip compare)

main = interact $ show . solve2 . parseInput
