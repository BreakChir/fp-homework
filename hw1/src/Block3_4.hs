{-# LANGUAGE InstanceSigs #-}

module Block3_4
  ( Tree (..)
  , fromList
  , isEmpty
  , insert
  , removeElem
  , search
  , size
  , toList
  ) where

import Block4 (NonEmpty (..))

data Tree a
  = Leaf
  | Branch (Tree a) (NonEmpty a) (Tree a)
  deriving (Show)

-- | Checks if the given tree is empty
-- >>> isEmpty Leaf
-- True
-- >>> isEmpty (Branch Leaf (5 :| []) Leaf)
-- False
isEmpty :: (Tree a) -> Bool
isEmpty Leaf = True
isEmpty _    = False

-- | Calculates size of the given tree
-- >>> size Leaf
-- 0
-- >>> let left = Branch Leaf (1 :| []) Leaf
-- >>> let right = Branch Leaf (6 :| [6]) Leaf
-- >>> size (Branch left (5 :| [5,5,5,5]) right)
-- 8
size :: (Tree a) -> Int
size Leaf                     = 0
size (Branch left list right) = neLength list + size left + size right
  where
    neLength :: NonEmpty a -> Int
    neLength (_ :| xs) = listLength xs 1

    listLength :: [a] -> Int -> Int
    listLength []     n = n
    listLength (_:ys) n = listLength ys (n + 1)

-- | Finds element in the given tree
-- >>> let left = Branch Leaf (3 :| []) Leaf
-- >>> let right = Branch Leaf (6 :| [6]) Leaf
-- >>> search 4 (Branch left (5 :| [5,5,5,5]) right)
-- Nothing
-- >>> search 6 (Branch left (5 :| [5,5,5,5]) right)
-- Just (Branch Leaf (6 :| [6]) Leaf)
search :: Ord a => a -> (Tree a) -> (Maybe (Tree a))
search _  Leaf                           = Nothing
search el b@(Branch left (x :| _) right)
  | el == x   = Just b
  | el < x    = search el left
  | otherwise = search el right

-- | Inserts element to the given tree
-- >>> let left = Branch Leaf (3 :| []) Leaf
-- >>> let right = Branch Leaf (7 :| [7]) Leaf
-- >>> insert 3 (Branch left (5 :| [5,5,5,5]) right)
-- Branch (Branch Leaf (3 :| [3]) Leaf) (5 :| [5,5,5,5]) (Branch Leaf (7 :| [7]) Leaf)
-- >>> insert 6 (Branch left (5 :| [5,5,5,5]) right)
-- Branch (Branch Leaf (3 :| []) Leaf) (5 :| [5,5,5,5]) (Branch (Branch Leaf (6 :| []) Leaf) (7 :| [7]) Leaf)
insert :: Ord a => a -> (Tree a) -> (Tree a)
insert el Leaf                               =
  Branch Leaf (el :| []) Leaf
insert el (Branch left list@(x :| xs) right)
  | el == x   = Branch left (x :| (el : xs)) right
  | el < x    = Branch (insert el left) list right
  | otherwise = Branch left list (insert el right)

-- | Gets tree from the given list
-- >>> fromList [1,1,1,0,2,3,0,3,2,1]
-- Branch (Branch Leaf (0 :| [0]) Leaf) (1 :| [1,1,1]) (Branch Leaf (2 :| [2]) (Branch Leaf (3 :| [3]) Leaf))
-- >>> fromList []
-- Leaf
fromList :: Ord a => [a] -> (Tree a)
fromList = foldr (\x tree -> insert x tree) Leaf

next :: (Tree a) -> (NonEmpty a, Tree a)
next Leaf                     = error "Unexpected Leaf"
next (Branch Leaf list right) = (list, right)
next (Branch left list right) = let (res, son) = next left
                                in (res, Branch son list right)

-- | Removes element from given tree
-- >>> let left = Branch Leaf (3 :| []) Leaf
-- >>> let right = Branch Leaf (7 :| [7]) Leaf
-- >>> removeElem 3 (Branch left (5 :| [5,5,5,5]) right)
-- (Branch Leaf (5 :| [5,5,5,5]) (Branch Leaf (7 :| [7]) Leaf),True)
-- >>> removeElem 6 (Branch left (5 :| [5,5,5,5]) right)
-- (Branch (Branch Leaf (3 :| []) Leaf) (5 :| [5,5,5,5]) (Branch Leaf (7 :| [7]) Leaf),False)
-- >>> removeElem 7 (Branch left (5 :| [5,5,5,5]) right)
-- (Branch (Branch Leaf (3 :| []) Leaf) (5 :| [5,5,5,5]) (Branch Leaf (7 :| []) Leaf),True)
removeElem :: Ord a => a -> (Tree a) -> (Tree a, Bool)
removeElem _  Leaf                               = (Leaf, False)
removeElem el (Branch left list@(x :| xs) right)
  | el == x   = let tree = case xs of
                             []     -> case (left, right) of
                                         (Leaf, _)    -> right
                                         (a,    Leaf) -> a
                                         (_,    _)    -> let (res, son) = next right
                                                         in Branch left res son
                             (_:ys) -> Branch left (x :| ys) right
                in (tree, True)
  | el < x    = let (tree, res) = (removeElem el left)
                in (Branch tree list right, res)
  | otherwise = let (tree, res) = (removeElem el right)
                in (Branch left list tree, res)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf                     = mempty
  foldMap f (Branch left list right) = foldMap f left <> foldMap f list <> foldMap f right

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ z Leaf                     = z
  foldr f z (Branch left list right) = foldr f (foldr f (foldr f z right) list) left

-- | Gets sorted list from given tree
-- >>> let left = Branch Leaf (0 :| [0]) Leaf
-- >>> let right = Branch Leaf (3 :| [3]) Leaf
-- >>> toList (Branch left (1 :| [1,1,1]) (Branch Leaf (2 :| [2]) right))
-- [0,0,1,1,1,1,2,2,3,3]
-- >>> toList Leaf
-- []
toList :: Tree a -> [a]
toList = foldr (:) []
