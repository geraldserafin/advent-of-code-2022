module Day10 where

import Text.Parsec (Parsec, many, anyChar)
import AOC (solve, solveWrite)
import Data.List.Split (chunksOf)

signal :: String -> Int
signal "addx" = 0
signal "noop" = 0
signal x      = read x

parser :: Parsec String () [Int]
parser = map signal . words <$> many anyChar

cycles :: [Int] -> [Int]
cycles = scanl1 (+) . ([1]++) 

part1 :: String -> IO ()
part1 = solve (sum . solution [20,60..220] . cycles) parser
  where 
    solution xs = map (\x -> (x*) . (!!(x-1)) $ xs)

part2 :: String -> IO ()
part2 = solveWrite (toString . pixels . cycles) parser
  where 
    pixels   = zipWith pixel (cycle [0..39]) 
    toString = unlines . map (concat) . chunksOf 40 
    pixel a b 
      | abs (a-b) <= 1 = "#"
      | otherwise      = " "
