{-# LANGUAGE ScopedTypeVariables #-}

module Task4
  ( factorial
  , fibonacci
  , iterateElement
  , mapFix
  ) where

fix :: (a -> a) -> a
fix f = let x = f x in x

iterateElement :: a -> [a]
iterateElement = fix $ \f x -> x : f x

fibonacci :: Integer -> Integer
fibonacci = fix fibonacciFix
  where
    fibonacciFix :: (Integer -> Integer) -> Integer -> Integer
    fibonacciFix f x
      | x < 0     = error "Expected Non negative argument"
      | x == 0    = 0
      | x == 1    = 1
      | otherwise = f (x - 1) + f (x - 2)

factorial :: Integer -> Integer
factorial = fix factorialFix
  where
    factorialFix :: (Integer -> Integer) -> Integer -> Integer
    factorialFix f x
      | x < 0     = error "Expected Non negative argument"
      | x <= 1    = 1
      | otherwise = x * f (x - 1)    
    
mapFix :: forall a b . (a -> b) -> [a] -> [b]
mapFix mapper = fix mapFixHelper
  where
    mapFixHelper :: ([a] -> [b]) -> [a] -> [b]
    mapFixHelper _ []     = []
    mapFixHelper f (x:xs) = mapper x : f xs
