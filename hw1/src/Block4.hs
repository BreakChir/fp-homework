{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Block4
  ( NonEmpty (..)
  , Pair (..)
  , joinWith
  , splitOn
  ) where

data Pair a
  = Pair a a

data NonEmpty a
  = a :| [a]
  deriving (Show)

instance Foldable Pair where
  foldr :: (a -> b -> b) -> b -> (Pair a) -> b
  foldr f z (Pair x y) =  f x $ f y z

  foldMap :: Monoid m => (a -> m) -> (Pair a) -> m
  foldMap f (Pair x y) = f x <> f y

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> (NonEmpty a) -> b
  foldr f z (x :| xs) = x `f` foldr f z xs

  foldMap :: Monoid m => (a -> m) -> (NonEmpty a) -> m
  foldMap f (x :| xs) = f x <> foldMap f xs

-- | Splits list by given element
-- >>> splitOn '/' "path/to/file"
-- "path" :| ["to","file"]
-- >>> splitOn '/' "///path/to/file"
-- "" :| ["","","path","to","file"]
-- >>> splitOn '/' "/path/to//fi/le//"
-- "" :| ["path","to","","fi","le","",""]
-- >>> splitOn '/' ""
-- "" :| []
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn el list = y :| ys
  where
    splitAcc :: a -> ([[a]], [a]) -> ([[a]], [a])
    splitAcc x (xs, acc)
      | x == el   = (acc : xs, [])
      | otherwise = (xs, x : acc)

    (ys, y) = foldr splitAcc ([], []) list

-- | Joins non empty list with given element
-- >>> joinWith '/' ("path" :| ["to","file"])
-- "path/to/file"
-- >>> joinWith '/' ("" :| ["","","path","to","file"])
-- "///path/to/file"
-- >>> joinWith '/' ("" :| ["path","to","","fi","le","",""])
-- "/path/to//fi/le//"
-- >>> joinWith '/' ("" :| [])
-- ""
joinWith :: a -> NonEmpty [a] -> [a]
joinWith el (l :| list) = l ++ (foldr (\x xs -> el : (x ++ xs)) [] list)
