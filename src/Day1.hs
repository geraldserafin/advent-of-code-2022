module Day1 (part2, part1) where

import AOC (solve)
import Parsers (number)
import Data.List (sort)
import Text.Parsec (Parsec, endOfLine, sepEndBy)

parser :: Parsec String () [[Int]]
parser = number `sepEndBy` endOfLine `sepEndBy` endOfLine

solution :: Show a => ([Int] -> a) -> FilePath -> IO ()
solution f = solve (f . map sum) parser

part1, part2 :: FilePath -> IO ()
part1 = solution maximum 
part2 = solution (sum . take 3 . reverse . sort)
