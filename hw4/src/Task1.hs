{-# LANGUAGE BangPatterns #-}

module Task1
  ( multiply
  ) where

import           Control.Monad.ST (ST, runST)

import           Data.Foldable (for_)
import           Data.STRef (newSTRef, modifySTRef, readSTRef)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

getDVector :: [[Int]] -> V.Vector (V.Vector Int)
getDVector list = V.fromList $ map V.fromList list

toDList :: Int -> V.Vector (M.MVector s Int) -> [[Int]] -> ST s [[Int]]
toDList i resMVec res
  | i < 0     = return res
  | otherwise = do
      let vec = resMVec V.! i
      vec1 <- V.freeze vec
      let list = V.toList vec1
      res1 <- toDList (i - 1) resMVec (list : res)
      return res1

multiply :: [[Int]] -> [[Int]] -> Maybe [[Int]]
multiply list1 list2 = do
  if (list1InnerSize /= list2Size)
    then Nothing
    else runST $ do
      innerVec <- M.replicate list2InnerSize 0 :: ST s (M.MVector s Int)
      vecM <- M.replicateM list1Size (M.clone innerVec)

      for_ [0..list1Size - 1] $ \i -> do
        vecMRow <- M.read vecM i
        for_ [0..list2InnerSize - 1] $ \j -> do
          let cell = multiplyCell i j
          M.write vecMRow j cell
        M.write vecM i vecMRow

      resVec <- V.freeze vecM
      res <- toDList (list1Size - 1) resVec []
      return (pure res)
  where
    list1Size, list2Size, list1InnerSize, list2InnerSize :: Int
    list1Size = length list1
    list2Size = length list2
    list1InnerSize = length $ head list1
    list2InnerSize = length $ head list2

    vec1, vec2 :: V.Vector (V.Vector Int)
    vec1 = getDVector list1
    vec2 = getDVector list2

    list2Index :: [Int]
    list2Index = [0..list2Size - 1]

    multiplyCell :: Int -> Int -> Int
    multiplyCell iRow iCol = runST $ do
      let row1 = vec1 V.! iRow
      res <- newSTRef 0
      for_ list2Index $ \i -> do
        let cell1 = row1 V.! i
        let row2 = vec2 V.! i
        let cell2 = row2 V.! iCol
        modifySTRef res (+ cell1 * cell2)
      readSTRef res
