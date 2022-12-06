module Day5 (part1, part2) where

import AOC (solve)
import Parsers (number)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.List.Split (sepBy)
import Text.Parsec hiding (space)
import Data.IntMap (IntMap, adjust, fromDistinctAscList, elems, (!))

type Crates = IntMap String
type Move   = (Int,Int,Int)

space :: Parsec String () Char
space = char ' '

instruction :: Parsec String () Move
instruction = do
  a <- string "move "  *> number
  b <- string " from " *> number
  c <- string " to "   *> number
  return (a, b, c)

crate :: Parsec String () (Maybe Char)
crate = Just    <$> (char '[' *> letter <* char ']') <|> 
        Nothing <$  (space *> count 2 anyChar)

crates :: Parsec String () (Crates)
crates = toIntMap <$> crate `sepBy1` space `sepEndBy1` endOfLine
  where
    toIntMap = fromDistinctAscList . zip [1..] . map (catMaybes) . transpose

parser :: Parsec String () (Crates, [Move])
parser = (,) <$> crates <*> (endOfLine *> instruction `sepBy1` endOfLine)

move :: ([a] -> [a]) -> IntMap [a] -> Move -> IntMap [a]
move g xs (n, a, b) =  adjust (drop n) a . adjust (elem++) b $ xs
  where
    elem = g . take n $ xs ! a

solution :: (String -> String) -> (Crates, [Move]) -> String
solution f (s, m) = map head . elems $ foldl (move f) s m

part1, part2 :: String -> IO ()
part1 = solve (solution reverse) parser
part2 = solve (solution id) parser
