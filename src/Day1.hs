module Day1 (part2, part1) where
import Data.List.Split (splitOn)
import Data.List (sort)

parseInput :: [Char] -> [Int]
parseInput s = map (sum . map read . splitOn "\n") $ splitOn "\n\n" s :: [Int]

part1 :: [Char] -> Int
part1 =  maximum . parseInput

part2 :: [Char] -> Int
part2 = sum . take 3 . reverse . sort . parseInput
