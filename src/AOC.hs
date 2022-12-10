module AOC (solve, solveWrite) where
  
import Text.Parsec (parse, Parsec, many1, string, Stream, ParsecT, digit)
import Data.List.Split (splitOn)

solveWrite :: (t -> String) -> Parsec String () t -> FilePath -> IO ()
solveWrite solution parser fileName = do
  f <- readFile fileName
  let outputDir = "output/" ++ (last $ splitOn "/" fileName)

  case parse parser "Parse error" f of 
    Left  err -> print err
    Right val -> writeFile outputDir $ solution val


solve :: Show a => (t -> a) -> Parsec String () t -> FilePath -> IO ()
solve solution parser fileName = do
  f <- readFile fileName

  case parse parser "Parse error" f of 
    Left  err -> print err
    Right val -> print $ solution val
