module Day9 (part1, part2, tPos) where

import AOC (solve)
import Data.List (nub)
import Data.List.Split (splitOn)
import Text.Parsec (Parsec, anyChar, many)

type Pos = (Int, Int)

initialPos :: Pos
initialPos = (0, 0)

parseMove :: Char -> Pos
parseMove 'L' = (0, -1)
parseMove 'R' = (0,  1)
parseMove 'U' = (1,  0)
parseMove 'D' = (-1, 0)

moveFromInput :: [Char] -> [Pos]
moveFromInput (d:_:r) = replicate (read r) $ parseMove d

hPos :: [String] -> [Pos]
hPos = scanl mergePos initialPos . concatMap moveFromInput 
  where 
    mergePos (a, b) (c, d) = (a+c, b+d)

tPos :: Pos -> Pos -> Pos
tPos (a, b) (c, d) 
  | all ((<=1) . abs) [x,y] = (a, b)
  | otherwise = (a + signum x, b + signum y)
  where 
    (x, y) = (c - a, d - b)

parser :: Parsec String () [String]
parser = lines <$> many anyChar

part1, part2 :: String -> IO ()
part1 = solve (length . nub . scanl1 tPos . hPos) parser
part2 = solve (length . nub . (!!9) . iterate (scanl1 tPos) . hPos) parser 
