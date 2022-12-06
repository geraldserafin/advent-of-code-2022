module Day2 (part1, part2) where
  
import AOC (solve)
import Parsers (split)
import Data.List.Split (splitOn)
import Text.Parsec (letter, endOfLine, sepBy1, Parsec)

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

outcomeScore :: Num a => (Char, Char) -> a
outcomeScore (a, b)
  | b == win a = 6 
  | b == a     = 3
  | otherwise  = 0

roundScore :: Num a => (Char, Char) -> a
roundScore (a, b) = score b + outcomeScore (a, b)

parser :: Parsec String () [(Char, Char)]
parser = split " " letter `sepBy1` endOfLine

part1, part2 :: FilePath -> IO ()
part1 = solve (sum . map (\(a, b) -> roundScore (a, translate b))) parser
part2 = solve (sum . map (\(a, b) -> roundScore (a, translate' b a))) parser
