module Day2 (part1, part2) where
import Data.List.Split (splitOn)

translate :: Char -> Char
translate 'X' = 'A'
translate 'Y' = 'B'
translate 'Z' = 'C'

translate' :: Char -> Char -> Char
translate' 'X' b = win $ win b
translate' 'Y' b = b
translate' 'Z' b = win b

score :: Num a => Char -> a
score 'A' = 1
score 'B' = 2
score 'C' = 3

win :: Char -> Char
win 'A' = 'B'
win 'B' = 'C'
win 'C' = 'A'

roundOutcome :: Num a => (Char, Char) -> a
roundOutcome (a, b)
  | b == win a = 6 
  | b == a     = 3
  | otherwise  = 0

roundScore :: Num a => (Char, Char) -> a
roundScore (a, b) = score b + roundOutcome (a, b)

parseInput :: ((Char, Char) -> b) -> [Char] -> [b]
parseInput f = map (f . (\x -> (head x, last x))) . splitOn "\n" 

part1 :: [Char] -> Integer
part1 = sum . parseInput (\(a, b) -> roundScore (a, translate b))

part2 :: [Char] -> Integer
part2 = sum . parseInput (\(a, b) -> roundScore (a, translate' b a))
