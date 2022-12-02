module Parser where

import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Char

import Lambda

type Parser = Parsec Void String

parseToken :: Parser String
parseToken = space *> some (satisfy isAlpha) <* space

parseVar :: Parser (Lambda String)
parseVar = Var <$> parseToken

parseInBrac :: Parser (Lambda String)
parseInBrac = char '(' *> parseTerm <*  char ')'

parseAbs :: Parser (Lambda String)
parseAbs = Abs <$> (char '\\' *> parseToken) <*> (char '.' *> parseTerm)

parseApp :: Parser (Lambda String)
parseApp = do
  x <- parseVar <|> parseInBrac
  _ <- space
  others <- (parseVar <|> parseInBrac <|> parseAbs) `sepEndBy` space
  return (apply (x:others)) where
    apply [x] = x
    apply (x:y:others) = apply (App x y : others)
    -- don't need case for [], as always call with at least `x`

parseTerm :: Parser (Lambda String)
parseTerm = space *> (try parseApp <|> try parseInBrac <|> try parseAbs <|> try parseVar) <* space


