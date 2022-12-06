module AOC (solve) where
  
import Text.Parsec (parse, Parsec, many1, string, Stream, ParsecT, digit)

solve :: Show a => (t -> a) -> Parsec String () t -> FilePath -> IO ()
solve solution parser fileName = do
  f <- readFile fileName

  case parse parser "Parse error" f of 
    Left  err -> print err
    Right val -> print $ solution val
