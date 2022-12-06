module Day6 (part1, part2) where

import AOC (solve)
import Data.List.Utils (uniq)
import Text.Parsec (many1, letter, Parsec)

parser :: Parsec String () String
parser = many1 letter 

findMarker :: Int -> String -> String
findMarker n (x:xs) 
  | marker $ ys = ys
  | otherwise   = x : findMarker n xs
  where 
    marker s = s == uniq s
    ys       =  take n (x:xs)

solution :: Int -> String -> IO ()
solution n = solve (length . findMarker n) parser

part1, part2 :: String -> IO ()
part1 = solution 4
part2 = solution 14
