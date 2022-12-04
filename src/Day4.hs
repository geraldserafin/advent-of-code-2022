module Day4 (part1, part2) where

import Data.List.Split (splitOn)

type Range = (Int, Int)

overlaps :: Range -> Range -> Bool
overlaps (a,b) (c,d) = a <= d && b >= c
        
isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf (a,b) (c,d) = a >= c && b <= d

parseInput :: String -> [(Range, Range)]
parseInput = map (toPair . getRanges . splitOn ",") . lines 
  where
    toPair [x, y] = (x, y) 
    getRanges = map (toPair . map read . splitOn "-")

solve :: ((Range, Range) -> Bool) -> String -> Int
solve f = length . filter f . parseInput 

part1 :: String -> Int
part1 = solve (\(a, b) -> a `isSubrangeOf` b || b `isSubrangeOf` a )

part2 :: String -> Int
part2 = solve (\(a, b) -> a `overlaps` b)
