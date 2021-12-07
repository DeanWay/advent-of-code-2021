module Puzzle2 where

import Data.Function (on)
import Data.List (group, minimumBy, sort)
import Puzzle1 (BitString, bitStringToInt, mostCommon, parse)

solve2 :: [BitString] -> Int
solve2 report = oxygenGeneratorRating' * c02ScrubberRating'
  where
    oxygenGeneratorRating' = bitStringToInt $ oxygenGeneratorRating report
    c02ScrubberRating' = bitStringToInt $ c02ScrubberRating report

oxygenGeneratorRating :: [BitString] -> BitString
oxygenGeneratorRating =
  iterateUntilOneRemains $
    filterReportOnBitsAtIterationNumber mostCommon

c02ScrubberRating :: [BitString] -> BitString
c02ScrubberRating =
  iterateUntilOneRemains $
    filterReportOnBitsAtIterationNumber leastCommon

filterReportOnBitsAtIterationNumber f (report, i) = (newReport, i + 1)
  where
    newReport = filter ((== keyValue) . (!! i)) report
    keyValue = f (map (!! i) report)

leastCommon :: Ord a => [a] -> a
leastCommon = head . minimumBy (compare `on` length) . group . sort

type StepFunction = ([BitString], Int) -> ([BitString], Int)

iterateUntilOneRemains :: StepFunction -> [BitString] -> BitString
iterateUntilOneRemains stepFunction report =
  head
    . head
    . filter ((== 1) . length)
    . map fst
    . iterate stepFunction
    $ (report, 0)

main = interact $ show . solve2 . parse
