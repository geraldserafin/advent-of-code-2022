module AOC (solve) where
import Text.Parsec (parse, Parsec)

solve :: Show a => (t -> a) -> Parsec String () t -> String -> IO ()
solve solution parser fileName = do
  f <- readFile fileName

  case parse parser "Parse error" f of 
    Left  err -> print err
    Right val -> print $ solution val

-- solve :: Show a => (t -> a) -> Parsec String () t -> String -> IO ()
-- solve part parser fileName = do
--   f <- readFile fileName

--   case parse parser "Parse error" f of 
--     Left  err -> print err
--     Right val -> print $ part val