module Block3_3
  ( bracketSequence
  , int
  , intMaybe
  , parserStream
  ) where

import qualified Data.Set as S
import           Control.Applicative (Alternative (..))

import           Block3_1 (Parser (..), first)
import           Block3_2 (element, eof, notOk, ok)

parserStream :: Parser s a -> Parser s [a]
parserStream p = Parser $ \s ->
  case runParser p s of
    Nothing      -> Just ([], s)
    Just (x, s') -> first (x :) <$> runParser (parserStream p) s'

bracketSequence :: Parser Char ()
bracketSequence = bracketSequence' <* eof >>= checkBalance
  where  
    bracketSequence' :: Parser Char Int
    bracketSequence' = Parser $ \s ->
      case runParser (element '(' <|> element ')') s of
        Nothing      -> Just (0, s)
        Just (c, s') -> first (bracketToInt c) <$> runParser bracketSequence' s'
    
    bracketToInt :: Char -> Int -> Int
    bracketToInt x z 
      | z <= 0    = if (x == '(') then z + 1 else z - 1
      | otherwise = z
    
    checkBalance :: Int -> Parser Char ()
    checkBalance z
      | z == 0    = ok
      | otherwise = notOk

someChar :: S.Set Char -> Parser Char Char
someChar cs = Parser $ \s ->
  case s of
    []     -> Nothing
    (x:xs) -> if (x `S.member` cs)
              then Just (x, xs)
              else Nothing

unSignInt :: Parser Char Int
unSignInt = read <$> ((:) <$> numChar <*> (parserStream numChar))
  where
    numChar :: Parser Char Char
    numChar = someChar $ S.fromList "0123456789"

int :: Parser Char Int
int = (sign <$> signChar <*> unSignInt) <|> unSignInt
  where
    signChar :: Parser Char Char
    signChar = someChar $ S.fromList "+-"

    sign :: Char -> Int -> Int
    sign x = if (x == '+') then id else (0 -)

intMaybe :: String -> Maybe Int
intMaybe s = fst <$> runParser (int <* eof) s
