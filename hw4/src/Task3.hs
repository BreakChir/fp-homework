{-# LANGUAGE BangPatterns #-}

module Task3
  ( gauss
  , verifySolution
  ) where

import           Control.Monad.ST (ST, runST)

import           Data.Foldable (foldr')
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M

type Row s = M.MVector s Bool
type Matrix s = M.MVector s (Row s)

gauss :: [[Bool]] -> [Bool] -> Maybe [Bool]
gauss a y = runST _gauss
  where
    m = length y
    n = length $ head a

    _gauss :: ST s (Maybe [Bool])
    _gauss = do
      innerList <- mapM (V.unsafeThaw . V.fromList) (concatAY a y)
      matrix <- (V.unsafeThaw . V.fromList) innerList
      elPlace <- M.replicate n (-1)
      ans <- calculate 0 0 matrix elPlace
      let hasSolution = verifySolution a y ans
      if hasSolution
        then return $ pure ans
        else return Nothing

    concatAY :: [[Bool]] -> [Bool] -> [[Bool]]
    concatAY []     []     = []
    concatAY (g:gs) (h:hs) = (g ++ [h]) : concatAY gs hs
    concatAY _      _      = []

    maxElem :: Int -> Int -> Matrix s -> ST s (Maybe Int)
    maxElem row col matrix
      | row == m  = return Nothing
      | otherwise = do
        rowRow <- M.read matrix row
        rowEl  <- M.read rowRow col
        if rowEl then return $ pure row else maxElem (row + 1) col matrix

    xorRow :: Int -> Row s -> Row s -> ST s (Row s)
    xorRow j iRow rowRow
      | j <= n    = do
        rowEl <- M.read rowRow j
        M.modify iRow (rowEl /=) j
        xorRow (j + 1) iRow rowRow
      | otherwise = return iRow

    toZero :: Int -> Int -> Int -> Matrix s -> ST s (Matrix s)
    toZero i row col matrix
      | i == m    = return matrix
      | i == row  = toZero (i + 1) row col matrix
      | otherwise = do
        iRow <- M.read matrix i
        iEl  <- M.read iRow col
        if (not iEl)
          then toZero (i + 1) row col matrix
          else do
            rowRow <- M.read matrix row
            iRow' <- xorRow col iRow rowRow
            M.write matrix i iRow'
            toZero (i + 1) row col matrix

    getAns :: Int -> Matrix s -> (M.MVector s Int) -> [Bool] -> ST s [Bool]
    getAns i matrix elPlace !res
      | i >= 0    = do
        el <- M.read elPlace i
        if (el /= -1)
          then do
            elRow <- M.read matrix el
            bEl <- M.read elRow n
            getAns (i - 1) matrix elPlace (bEl : res)
          else getAns (i - 1) matrix elPlace (False : res)
      | otherwise = return res

    calculate :: Int -> Int -> Matrix s -> (M.MVector s Int) -> ST s [Bool]
    calculate row col matrix elPlace
      | row < m && col < n = do
        pivotRow <- maxElem row col matrix
        case pivotRow of
          Nothing  -> calculate row (col + 1) matrix elPlace
          Just sel -> do
            M.swap matrix row sel
            M.write elPlace col row
            matrix' <- toZero 0 row col matrix
            calculate (row + 1) (col + 1) matrix' elPlace
      | otherwise          = getAns (n - 1) matrix elPlace []

verifySolution :: [[Bool]] -> [Bool] -> [Bool] -> Bool
verifySolution a y x = calculateMatrix
  where
    m = length y
    n = length x
    yVec = V.fromList y
    xVec = V.fromList x
    aVec = V.fromList $ map V.fromList a

    calculateCol ai j = (ai V.! j) && (xVec V.! j)
    calculateRow i = foldr' (/=) False (map (calculateCol (aVec V.! i)) [0..n - 1])
    calculateRow' i = (calculateRow i) == (yVec V.! i)
    calculateMatrix = foldr' (&&) True (map calculateRow' [0..m - 1])
