module Day4 (part1, part2) where
  
import AOC (solve)
import Parsers (split, number)
import Text.Parsec (Parsec, endOfLine, sepBy1, many)

type Range = (Int, Int)

parser :: Parsec String () [(Range, Range)]
parser = concat <$> many (split "," (split "-" number)) `sepBy1` endOfLine

overlaps :: Range -> Range -> Bool
overlaps (a,b) (c,d) = a <= d && b >= c
        
isSubrangeOf :: Range -> Range -> Bool
isSubrangeOf (a,b) (c,d) = a >= c && b <= d

solution :: ((Range, Range) -> Bool) -> String -> IO ()
solution f = solve (length . filter f) parser

part1, part2 :: String -> IO ()
part1 = solution (\(a, b) -> a `isSubrangeOf` b || b `isSubrangeOf` a )
part2 = solution (\(a, b) -> a `overlaps` b)
