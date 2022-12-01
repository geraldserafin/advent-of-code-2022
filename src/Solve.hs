module Solve (solve) where

solve :: Show b => (String -> b) -> String -> IO ()
solve part fileName = do
  f <- readFile fileName
  print $ part f 