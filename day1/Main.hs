import Puzzle2 (solve2)

parse :: String -> [Int]
parse = map read . lines

main = interact $ show . solve2 . parse
