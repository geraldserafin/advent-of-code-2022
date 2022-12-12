module Day12 where

import AOC (solve)
import Data.Char (ord)
import Data.List (elemIndex)
import Algorithm.Search (bfs)
import Data.List.HT (replace)
import qualified Data.Map as Map
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust, mapMaybe)
import Text.Parsec (Parsec, many, anyChar)

find s xs = fromJust $ elemIndex s xs
findAll s = map fst . filter (\(_, b) -> b == s) . zip [0..]

solution :: Char -> [String] -> [Maybe [Int]]
solution sp xs = map (bfs (graph Map.!) (==e)) starts
  where
    g = concat xs
    w = length $ head xs
    i = replace "E" "z" $ replace "S" "a" g

    starts = findAll sp g

    graph  = Map.fromList $ map (\x -> (x, neighbours x)) [0..length i-1]
    (s, e) = (find 'S' g, find 'E' g)
    
    neighbours x = canGo x $ filter (`elem` [0..length i-1]) [x-1, x+1, x-w, x+w]
    canGo      x = filter (\a -> ord (i!!x) - ord (i!!a) >= -1)

parser :: Parsec String () [String]
parser = lines <$> many anyChar

part1 = solve (minimum . mapMaybe (fmap length) . solution 'a') parser
part2 = solve (minimum . mapMaybe (fmap length) . solution 'S') parser