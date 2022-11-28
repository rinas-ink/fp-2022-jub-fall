{-# LANGUAGE InstanceSigs #-}

module Combinators where

import Control.Applicative

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Just (input', res) -> Just (input', f res)
      Nothing -> Nothing

instance Applicative Parser where
  -- Parser that does nothing.
  pure :: a -> Parser a
  pure x = Parser $ \input -> Just (input, x)

  -- xs, xs', xs'' :: String
  -- Return function that at first applies Parser (a -> b), gets some function g :: a -> b
  -- then applies Parser a and applies g to the result
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser u <*> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> Nothing
      Just (xs', g) ->      -- g :: (a -> b)
        case v xs' of
          Nothing -> Nothing
          Just (xs'', x) -> Just (xs'', g x)

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser u <|> Parser v = Parser $ \xs ->
    case u xs of
      Nothing -> v xs
      z -> z

-- Creates function that bites off the first character if it satisfies the condition or function that returns Nothing. Parser itself is a function.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
  Parser f
  where
    f (c : cs) | p c = Just (cs, c)
    f _ = Nothing

char :: Char -> Parser Char
char p = satisfy (== p)

-- Apply parsers from array until you found the which worked
anyOf :: [Parser a] -> Parser a
anyOf arr = foldl (<|>) empty arr

-- first [(sep, second), (sep', third)] -> sep' (sep (first, second)) third
-- To prefix notation
leftAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
leftAssoc f (first, rest) =
  -- go through rest (it's list) and wrap it like above. f helps to wrap.
  foldl (\acc (sep, elem) -> f acc sep elem) first rest

rightAssoc :: (elem -> sep -> elem -> elem) -> (elem, [(sep, elem)]) -> elem
rightAssoc f (first, rest) =
  let (beginning, last) = go (first, rest)
   in foldr (\(elem, sep) acc -> f elem sep acc) last beginning
  where
    go :: (elem, [(sep, elem)]) -> ([(elem, sep)], elem)
    go (first, []) = ([], first)
    go (first, ((sep, second) : rest)) =
      let (list, last) = go (second, rest)
       in ((first, sep) : list, last)

-- From parser of element and separator we get parser for sequence of them
list :: Parser elem -> Parser sep -> Parser (elem, [(sep, elem)])
list elem sep =
  -- Function parses element and then parses things to array of 0 or more pairs.
  (,) <$> elem <*> many ((,) <$> sep <*> elem)