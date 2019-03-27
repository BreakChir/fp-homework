{-# LANGUAGE InstanceSigs #-}

module Block3_1
  ( Parser (..)
  , first
  ) where

import Control.Applicative (Alternative (..))

first :: (a -> c) -> (a, b) -> (c, b)
first f (r, s) = (f r, s)

data Parser s a = Parser
  { runParser :: [s] -> Maybe (a, [s])
  }

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser parser) = Parser (fmap (first f) . parser)

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser pf <*> Parser pa = Parser $ \s -> do
    (f, t) <- pf s
    (a, r) <- pa t
    pure (f a, r)

instance Monad (Parser s) where
  return :: a -> Parser s a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser pa >>= f = Parser $ \s -> do
    (a, t) <- pa s
    runParser (f a) t

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser pa <|> Parser pb = Parser $ \s -> pa s <|> pb s
