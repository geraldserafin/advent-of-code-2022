module Day11 (part1, part2) where
  
import AOC (solve)
import Parsers (number)
import Data.List (sort)
import Data.Bool.HT ( (?:) )
import Data.String.Utils (strip)
import Text.Parsec (Parsec, (<|>), string, digit, char, many1, spaces, sepBy, option, space)

data Monkey = Monkey { 
  monkeyId  ::  Int, 
  items     :: [Int], 
  operation ::  Int -> Int, 
  testNum   ::  Int,
  receivers :: (Int, Int),
  score     ::  Int
}

parseId :: Parsec String () Int
parseId = string "Monkey " *> number <* char ':'

parseItems :: Parsec String () [Int]
parseItems = spaces *> string "Starting items: " *> number `sepBy` string ", "

matchOp :: Char -> String -> (Int -> Int)
matchOp '*' "old" = (^ 2)
matchOp '*'   n   = (* read n)
matchOp '+'   n   = (+ read n)

parseOp :: Parsec String () (Int -> Int)
parseOp = do
  spaces *> string "Operation: new = old "
  (op:_) <- string "* "  <|> string "+ "
  num    <- string "old" <|> many1 digit
  return (matchOp op num)

parseTest :: Parsec String () Int
parseTest = spaces *> string "Test: divisible by " *> number

parseReceiver :: Parsec String () (Int, Int)
parseReceiver = do
  m1 <- spaces *> string "If true: throw to monkey "  *> number 
  m2 <- spaces *> string "If false: throw to monkey " *> number 
  return (m1, m2)

parseMonkey :: Parsec String () Monkey
parseMonkey = do
  m <- parseId
  i <- parseItems
  o <- parseOp
  t <- parseTest
  r <- parseReceiver
  return (Monkey m i o t r 0)

parser :: Parsec String () [Monkey]
parser = parseMonkey `sepBy` spaces

solution :: String -> Int -> [Monkey] -> Int
solution p n ms = monkeyBusiness . (!!n) $ iterate round' ms
  where 
    lcm'    = foldl1 lcm $ map testNum ms
    reduce' = if p == "p1" then flip div 3 else flip mod lcm'

    assignItems :: Monkey -> [(Int, Int)]
    assignItems m = zip newItems $ map assignee newItems
      where 
        assignee = flip (?:) (receivers m) . (0==) . flip mod (testNum m)
        newItems = map (reduce' . operation m) $ items m 
      
    inspect :: [Monkey] -> [Monkey]
    inspect (m:ms) = give ms newItems ++ [m { items = [], score = mScore }]
      where 
        mScore    = score m + length (items m)
        newItems  = assignItems m
        keep m    = map fst . filter (\(a,b) -> b == monkeyId m) 
        give xs i = map (\m -> m { items = items m ++ keep m i }) ms 

    round' :: [Monkey] -> [Monkey]
    round' xs = (!!length xs) $ iterate inspect xs

    monkeyBusiness :: [Monkey] -> Int
    monkeyBusiness = product . take 2 . reverse . sort . map score 

part1, part2 :: String -> IO ()
part1 = solve (solution "p1" 20   ) parser
-- TODO: figure out a better way of handling parts
part2 = solve (solution "p2" 10000) parser
