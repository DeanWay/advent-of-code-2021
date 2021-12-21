module Puzzle1 where

import Control.Monad
import Data.Function
import Data.Functor.Identity
import Text.Parsec

between' :: Char -> Char -> ParsecT String u Identity a -> ParsecT String u Identity a
between' c1 c2 = between (char c1) (char c2 <|> (do eof >> return ' '))

braces = between' '{' '}'

parens = between' '(' ')'

angleBraces = between' '<' '>'

squareBraces = between' '[' ']'

manyBalanced = skipMany wrapping >> eof
  where
    wrapping =
      parens (skipMany wrapping)
        <|> braces (skipMany wrapping)
        <|> squareBraces (skipMany wrapping)
        <|> angleBraces (skipMany wrapping)

parseLine = parse manyBalanced ""

lineScore line =
  case parseLine line of
    (Left error) -> errorScore error
    (Right _) -> 0
  where
    errorScore err =
      scoreForChar $ line !! (sourceColumn (errorPos err) - 1)
    scoreForChar c
      | c == ')' = 3
      | c == ']' = 57
      | c == '}' = 1197
      | c == '>' = 25137
      | otherwise = error $ show c

printParse line =
  case parseLine line of
    (Left error) -> print error
    (Right _) -> print "ok"

main = do
  lines' <- lines <$> getContents
  forM_ lines' printParse
  print $ sum . map lineScore $ lines'
