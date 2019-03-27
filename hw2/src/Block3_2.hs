module Block3_2
  ( element
  , eof
  , notOk
  , ok
  , satisfy
  , stream
  ) where

import Block3_1 (Parser (..))

ok :: Parser s ()
ok = Parser $ \s -> Just ((), s)

notOk :: Parser s ()
notOk = Parser $ \_ -> Nothing

eof :: Parser s ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), [])
    _  -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s ->
  case s of
    []     -> Nothing
    (x:xs) -> if (p x) then Just(x, xs) else Nothing

element :: Char -> Parser Char Char
element c = satisfy (c ==)

stream :: String -> Parser Char String
stream str = Parser $ \s -> strEquals str s
  where
    strEquals :: String -> String -> Maybe (String, String)
    strEquals [] t          = Just (str, t)
    strEquals (x:xs) (y:ys)
      | x == y              = strEquals xs ys
    strEquals _      _      = Nothing
