module Day10 where

import AOC (solve, solveWrite)
import Data.List.Split (chunksOf)
import Text.Parsec (Parsec, many, anyChar)

signal :: String -> Int
signal "addx" = 0
signal "noop" = 0
signal x      = read x

parser :: Parsec String () [Int]
parser = map signal . words <$> many anyChar

cycles :: [Int] -> [Int]
cycles = scanl1 (+) . ([1]++) 

part1 :: String -> IO ()
part1 = solve (sum . every 40 . drop 19 . zipWith (*) [1..] . cycles) parser
  where 
    every n [] = []
    every n as = head as : (every n $ drop n as)

part2 :: String -> IO ()
part2 = solveWrite (toString . pixels . cycles) parser
  where 
    pixels   = zipWith pixel $ cycle [0..39]
    toString = unlines . map concat . chunksOf 40 
    pixel a b 
      | abs (a-b) <= 1 = "#"
      | otherwise      = " "
