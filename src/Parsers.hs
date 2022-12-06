{-# LANGUAGE FlexibleContexts #-}
module Parsers (split, number) where

import Text.Parsec (Parsec, Stream, ParsecT, many1, digit, string, char)

number :: Parsec String () Int
number = read <$> many1 digit

split :: Stream s m Char => String -> ParsecT s u m a -> ParsecT s u m (a, a)
split str parser = (,) <$> parser <*> (string str *> parser)
