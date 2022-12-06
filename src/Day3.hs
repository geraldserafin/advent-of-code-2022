module Day3 (part1, part2) where

import AOC (solve)
import Data.List (intersect)
import Data.Char (isLower, ord)
import Data.List.Split (chunksOf)
import Text.Parsec (letter, many, endOfLine, sepBy1, Parsec)

parser :: Parsec String () [String]
parser = (many letter)`sepBy1` endOfLine

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

-- assuming even length
halve :: [a] -> [[a]]
halve xs = chunksOf (div (length xs) 2) xs

solution :: Foldable t => [t [Char]] -> Int
solution = sum . map (priority . head . foldr1 intersect) 

part1, part2 :: FilePath -> IO ()
part1 = solve (solution . map halve) parser
part2 = solve (solution . chunksOf 3) parser
