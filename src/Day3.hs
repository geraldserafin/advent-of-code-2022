module Day3 (part1, part2) where

import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (isLower, ord)

priority :: Char -> Int
priority c
  | isLower c = ord c - ord 'a' + 1
  | otherwise = ord c - ord 'A' + 27

-- assuming even length
halve :: [a] -> [[a]]
halve xs = chunksOf (div (length xs) 2) xs

solve :: Foldable t => ([String] -> [t String]) -> String -> Int
solve f = sum . map (priority . head . foldr1 intersect) . f . lines  

part1 :: String -> Int
part1 = solve (map halve)

part2 :: String -> Int
part2 = solve (chunksOf 3) 
