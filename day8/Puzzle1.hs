module Puzzle1 where

import Control.Monad (liftM2)
import Data.Set (Set, fromList, size)
import Text.Parsec
  ( char,
    many,
    many1,
    newline,
    oneOf,
    parse,
  )

solve1 :: [([Set Char], [Set Char])] -> Int
solve1 input = num1478
  where
    outputs = map snd input
    num1478 = count (is1 <||> is4 <||> is7 <||> is8) $ concat outputs

is1 = (== 2) . size

is4 = (== 4) . size

is7 = (== 3) . size

is8 = (== 7) . size

(<||>) = liftM2 (||)

count pred = length . filter pred

main = do
  parsedLines <- parseInput <$> getContents
  case parsedLines of
    (Left error) -> print error
    (Right input) -> print $ solve1 input

-- fancy parsing
screenSegment = oneOf "abcdefg"

screenDigit = many1 screenSegment

spaceChars = many (char ' ')

screenDigits = many (screenDigit <* spaceChars)

puzzleLine = do
  spaceChars
  keyDigits <- screenDigits
  spaceChars
  char '|'
  spaceChars
  outputDigits <- screenDigits
  spaceChars
  return (map fromList keyDigits, map fromList outputDigits)

puzzleLines = many (puzzleLine <* newline)

parseInput = parse puzzleLines ""
