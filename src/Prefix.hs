module Prefix where

import Lexer
import Expr
import Combinators
import Control.Applicative

-- Expr :: + Expr Expr
--       | * Expr Expr
--       | Digit -- Только одинокие цифры; пробелы не разрешены
-- +1*234 -> Just ("4", ...)
parse :: String -> Maybe (String, Expr)
parse = runParser parsePrefix

-- <*> and <$> are left assotiative.
-- (BinOp <$> op) :: Parser (Expr -> Expr) - it wraps the second number of a Parser pair into Expr. 
-- <*> - When applied to a string they are called one after another.
parsePrefix :: Parser Expr
parsePrefix =
        ((BinOp <$> op) <*> parsePrefix <*> parsePrefix)
    <|> Number <$> digit
  where
    -- function that can parse any of given ops
    op = anyOf [plus, minus, star, division, hat]