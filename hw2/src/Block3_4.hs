module Block3_4
  ( intLists
  ) where

import Block3_1 (Parser (..), first)
import Block3_2 (element, eof)
import Block3_3 (int, parserStream)

ignoreSpace :: Parser Char [Char]
ignoreSpace = parserStream (element ' ')

intIgnoreSpace :: Parser Char Int
intIgnoreSpace = ignoreSpace *> int

comma :: Parser Char Char
comma = ignoreSpace *> element ','

intList :: Parser Char [Int]
intList = intIgnoreSpace >>= parseList (comma *> intIgnoreSpace)
  where
    parseList :: Parser Char Int -> Int -> Parser Char [Int]
    parseList p n
      | n < 0     = Parser $ \_ -> Nothing
      | n == 0    = pure []
      | otherwise = (:) <$> p <*> parseList p (n - 1)

intListsComb :: Parser Char [[Int]]
intListsComb = Parser $ \s ->
  case runParser intList s of
    Nothing         -> Just ([], s)
    Just (list, s') ->
      case runParser comma s' of
        Nothing     -> Just ([list], s')
        Just (_, t) -> first (list :) <$> runParser intListsComb t

intLists :: Parser Char [[Int]]
intLists = intListsComb <* (ignoreSpace *> eof)
