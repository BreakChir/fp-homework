{-# LANGUAGE InstanceSigs #-}

module Block1
  ( NonEmpty (..)
  , Tree (..)
  , stringSum
  ) where

import Block3_3 (intMaybe)

stringSum :: String -> Maybe Int
stringSum str = sum <$> (traverse intMaybe $ words str)

data Tree a 
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show)

instance Eq a => Eq (Tree a) where
  (==) :: Tree a -> Tree a -> Bool
  Leaf x              == Leaf y              = x == y
  Branch left1 right1 == Branch left2 right2 = left1 == left2 && right1 == right2
  _                   == _                   = False

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf x)     = Leaf $ f x
  fmap f (Branch a b) = Branch (fmap f a) (fmap f b)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (Leaf f)     <*> br = f <$> br
  (Branch a b) <*> br = Branch (a <*> br) (b <*> br)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf x)     = f x z
  foldr f z (Branch a b) = foldr f (foldr f z b) a

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf x)     = Leaf <$> f x
  traverse f (Branch a b) = Branch <$> traverse f a <*> traverse f b

data NonEmpty a
  = a :| [a]
  deriving (Show)

instance Eq a => Eq (NonEmpty a) where
  (==) :: NonEmpty a -> NonEmpty a -> Bool
  x :| xs == y :| ys = x == y && xs == ys

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (a :| list) = f a :| (f <$> list)

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  f :| fs <*> x :| xs = f x :| ((f <$> xs) ++ (fs <*> x : xs))

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs
      
instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  z :| zs >>= f = y :| (ys ++ foldr ((++) . toList . f) [] zs)
    where
      y :| ys = f z

      toList :: NonEmpty a -> [a]
      toList (x :| xs) = x : xs
