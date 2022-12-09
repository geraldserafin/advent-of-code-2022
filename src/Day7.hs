module Day7 (part1, part2) where

import AOC (solve)
import Data.List (isPrefixOf, sort)
import Text.Parsec (anyChar, many, Parsec)

data Command = Cd String | File Int
data Tree a  = Tree String [Tree a]   | Leaf a   deriving Show

insert :: [String] -> [Tree Int] -> Tree Int -> Tree Int
insert   []   elems (Tree path sub) = Tree path (sub ++ elems)
insert (x:xs) elems (Tree path sub) = Tree path (map f sub)
  where
    f (Leaf i) = (Leaf i)
    f (Tree path' sub')
      | path' == x = insert xs elems (Tree path' sub')
      | otherwise  = (Tree path' sub')

matchLine :: String -> Command
matchLine command 
  | "$ cd" `isPrefixOf` command = Cd (last parts)
  | otherwise = File (read $ head parts)
  where 
    parts = words command

alterTree :: ([String], Tree Int) -> Command -> ([String], Tree Int)
alterTree (path, tree) (File size) = (path, insert path [Leaf size] tree)
alterTree (path, tree) (Cd dir)
  | dir == ".." = (init path, tree)
  | otherwise   = (path ++ [dir], insert path [Tree dir []] tree)

treeFromString :: [String] -> Tree Int
treeFromString = snd . foldl (alterTree) initTree . map matchLine . filterCommands
  where 
    initTree       = ([], (Tree "" []))
    filterCommands = filter (not . isPrefixOf "dir") . filter (/="$ ls")

sumTree :: Num a => Tree a -> a
sumTree (Leaf size)    = size
sumTree (Tree _ nodes) = sum $ map (sumTree) nodes

listTrees :: Tree a -> [Tree a]
listTrees (Leaf s)   = []
listTrees (Tree n s) = [Tree n s] ++ (concatMap listTrees s)

solution' :: [String] -> Int
solution' xs = head . dropWhile (<required) . reverse $ spaces
  where
    spaces   = map (sumTree) . drop 1 . listTrees $ treeFromString xs
    used     = maximum spaces
    unused   = 70000000 - used
    required = 30000000 - unused

solution :: [String] -> Int
solution = sum . filter (<100000) . map (sumTree) . drop 1 . listTrees . treeFromString

parser :: Parsec String () [String]
parser = lines <$> many anyChar

part1, part2 :: FilePath -> IO ()
part1 = solve solution parser
part2 = solve solution' parser
