module Puzzle2 where

import Data.Either
import Data.List
import Data.Maybe

data Balanced = Balanced {open :: Char, close :: Char}

wrappings =
  [ Balanced {open = '(', close = ')'},
    Balanced {open = '{', close = '}'},
    Balanced {open = '[', close = ']'},
    Balanced {open = '<', close = '>'}
  ]

closingOf :: Char -> Char
closingOf c = close . fromJust . find ((== c) . open) $ wrappings

closes closingC openingC = case find ((== openingC) . open) wrappings of
  Nothing -> False
  Just balanced -> closingC == close balanced

isOpen :: Char -> Bool
isOpen c = elem c $ map open wrappings

isClose :: Char -> Bool
isClose c = elem c $ map close wrappings

type Unparsed = String

type RemainingOpen = String

parseBalanced :: String -> Either Unparsed RemainingOpen
parseBalanced s = recurse s []
  where
    recurse [] stack = Right stack
    recurse (x : rest) stack
      | isOpen x = recurse rest (x : stack)
      | isClose x =
        if not (null stack) && x `closes` head stack
          then recurse rest (tail stack)
          else Left (x : rest)

formatOutput = either ("Fail, unparsed: " ++) ("Success, remainingOpen:" ++)

solve2 :: [String] -> Int
solve2 =
  floor
    . median
    . map (foldl (\acc x -> acc * 5 + x) 0)
    . map (map (scoreForChar . closingOf))
    . rights
    . map parseBalanced

scoreForChar c
  | c == ')' = 1
  | c == ']' = 2
  | c == '}' = 3
  | c == '>' = 4

main =
  interact $
    show . solve2 . lines

median xs
  | odd len = sorted !! mid
  | even len = (sorted !! (mid - 1) + sorted !! mid) / 2
  where
    sorted = sort xs
    len = length sorted
    mid = len `div` 2
