import Puzzle1 (finalPosition, parse)
import Puzzle2

main = interact $ show . solve2 . parse
