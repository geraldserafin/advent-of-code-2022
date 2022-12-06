module Day5 (part1, part2) where

import Data.List (transpose)
import Data.String.Utils (strip)
import Data.List.Split (chunksOf, sepBy)
import Data.IntMap (IntMap, adjust, fromDistinctAscList, elems, (!))
import Text.Parsec hiding (space)
import Data.Maybe (catMaybes)
import AOC (solve)

type Crates = IntMap String
type Move   = (Int,Int,Int)

space :: Parsec String () Char
space = char ' '

instruction :: Parsec String () Move
instruction = do
  a <- string "move "  *> many1 digit
  b <- string " from " *> many1 digit
  c <- string " to "   *> many1 digit
  return (read a, read b, read c)

instructions :: Parsec String () [Move]
instructions = instruction `sepBy1` endOfLine 

crate :: Parsec String () (Maybe Char)
crate = Just    <$> (char '[' *> letter <* char ']') <|> 
        Nothing <$  (space *> count 2 anyChar)

crates :: Parsec String () (Crates)
crates = toIntMap <$> crate `sepBy1` space `sepEndBy1` endOfLine
  where
    toIntMap = fromDistinctAscList . zip [1..] . map (catMaybes) . transpose

parser :: Parsec String () (Crates, [Move])
parser = do
  c <- crates
  endOfLine
  i <- instructions
  return (c, i)

move :: ([a] -> [a]) -> IntMap [a] -> Move -> IntMap [a]
move g xs (n, a, b) =  adjust (drop n) a . adjust (elem++) b $ xs
  where
    elem = g . take n $ xs ! a

solution :: (String -> String) -> (Crates, [Move]) -> String
solution f (s, m) = map head . elems $ foldl (move f) s m

part1 :: String -> IO ()
part1 = solve (solution reverse) parser

part2 :: String -> IO ()
part2 = solve (solution id) parser