module Block1
  ( contains
  , order3
  , smartReplicate
  , stringSum
  ) where

import Data.List(sort)

-- | Sorts third-element tuple
-- >>> order3 (5, 2, 10)
-- (2,5,10)
-- >>> order3 (1, 1, 1)
-- (1,1,1)
-- >>> order3 (9.3, 10.21, 9.1)
-- (9.1,9.3,10.21)
order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 (a, b, c) = (x, y, z)
  where
    [x, y, z] = sort [a, b, c]

-- | Replicates element, which equals `x`, `x` times
-- >>> smartReplicate [1,2,3]
-- [1,2,2,3,3,3]
-- >>> smartReplicate [0,2,-1,0]
-- [2,2]
-- >>> smartReplicate []
-- []
smartReplicate :: (Num a, Ord a) => [a] -> [a]
smartReplicate = concatMap (\x -> replicateNumber x x [])
  where
    replicateNumber :: (Num a, Ord a) => a -> a -> [a] -> [a]
    replicateNumber x k acc
      | k > 0     = replicateNumber x (k - 1) (x : acc)
      | otherwise = acc

-- | Finds lists, which contains given element
-- >>> contains 3 [[1..5], [2,0], [3,4]]
-- [[1,2,3,4,5],[3,4]]
-- >>> contains 3 [[], [2,1,33], [3,5]]
-- [[3,5]]
-- >>> contains 3.14 []
-- []
contains :: Eq a => a -> [[a]] -> [[a]]
contains x = filter $ elem x

-- | Calculates sum of numbers, which separated whitespaces
-- >>> stringSum "1 1"
-- 2
-- >>> stringSum "100\n\t-3"
-- 97
-- >>> stringSum ""
-- 0
-- >>> stringSum "\n\t100\t-200\n\t2\t1\n-3\t100\n\t"
-- 0
stringSum :: String -> Int             
stringSum str = sum $ map read $ words str
