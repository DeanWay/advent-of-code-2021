module Puzzle2 where

import Data.List (find)
import Data.Maybe (fromJust)
import Data.Set (Set, isSubsetOf, size)
import Puzzle1 (is1, is4, is7, is8, parseInput)

solve2 :: [([Set Char], [Set Char])] -> Int
solve2 = sum . map solveLine

solveLine :: ([Set Char], [Set Char]) -> Int
solveLine inputLine = read $ map getValue outputs
  where
    constraints = fst inputLine
    outputs = snd inputLine
    derive pred = fromJust $ find pred constraints
    zero = derive (is0 five)
    one = derive is1
    two = derive (is2 three five)
    three = derive (is3 one)
    four = derive is4
    five = derive (is5 six)
    six = derive (is6 one)
    seven = derive is7
    eight = derive is8
    nine = derive (is9 four)
    getValue x
      | x == zero = '0'
      | x == one = '1'
      | x == two = '2'
      | x == three = '3'
      | x == four = '4'
      | x == five = '5'
      | x == six = '6'
      | x == seven = '7'
      | x == eight = '8'
      | x == nine = '9'

is0 five chars = size chars == 6 && not (five `isSubsetOf` chars)

is2 three five chars = size chars == 5 && chars /= three && chars /= five

is3 one chars = size chars == 5 && (one `isSubsetOf` chars)

is5 six chars = size chars == 5 && (chars `isSubsetOf` six)

is6 one chars = size chars == 6 && not (one `isSubsetOf` chars)

is9 four chars = size chars == 6 && (four `isSubsetOf` chars)

main = do
  parsedLines <- parseInput <$> getContents
  case parsedLines of
    (Left error) -> print error
    (Right input) -> print $ solve2 input
