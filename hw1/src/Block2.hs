{-# LANGUAGE ScopedTypeVariables #-}

module Block2
  ( deleteElemByIndex
  , mergeSort
  ) where

-- | Deletes element by given index
-- >>> deleteElemByIndex 2 [1,2,3,4]
-- Just (3,[1,2,4])
-- >>> deleteElemByIndex (-2) [1,5,9,0]
-- Nothing
-- >>> deleteElemByIndex 0 [1]
-- Just (1,[])
deleteElemByIndex :: forall a . Int -> [a] -> Maybe (a, [a])
deleteElemByIndex n _
  | n < 0                     = Nothing
deleteElemByIndex _ []        = Nothing
deleteElemByIndex 0 (x:xs)    = Just (x, xs)
deleteElemByIndex n (x:xs)    = fmap fmapper (deleteElemByIndex (n - 1) xs)
  where
    fmapper :: (a, [a]) -> (a, [a])
    fmapper (a, ns) = (a, x : ns)

-- | Sorts given list using mergeSort
-- >>> mergeSort [2,1,0,3,10,5]
-- [0,1,2,3,5,10]
-- >>> mergeSort [1,2,3,4]
-- [1,2,3,4]
-- >>> mergeSort [1.6,2.3,0.1,1.78]
-- [0.1,1.6,1.78,2.3]           
mergeSort :: Ord a => [a] -> [a]
mergeSort []   = []
mergeSort [x]  = [x]
mergeSort list = innerSort lSort rSort
  where
    (left, right) = split list
    lSort = mergeSort left
    rSort = mergeSort right

    innerSort :: Ord a => [a] -> [a] -> [a]
    innerSort xs []               = xs
    innerSort [] ys               = ys
    innerSort lx@(x:xs) ly@(y:ys)
      | x < y     = x : (innerSort xs ly)
      | otherwise = y : (innerSort ys lx)

    split :: [a] -> ([a], [a])
    split []       = ([], [])
    split [x]      = ([x],[])
    split (x:y:xs) = let (ns, ms) = split xs
                     in (x : ns, y : ms)
