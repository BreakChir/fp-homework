module Task1Spec
  ( spec
  ) where

import Data.List (transpose)

import Task1 (multiply)

import Criterion.Main (bench, bgroup, defaultMain, nf)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, generate, vectorOf)

getIntList :: Int -> Gen [Int]
getIntList size = do
  let genInt = choose (-10000, 10000)
  list <- vectorOf size genInt
  return list

getMatrix :: Int -> Int -> Gen [[Int]]
getMatrix rows cols = do
  let genList = getIntList cols
  list <- vectorOf rows genList
  return list

multiplyNaive :: Num a => [[a]] -> [[a]] -> [[a]]
multiplyNaive a b = [[sum $ zipWith (*) x y | y <- transpose b] | x <- a]

spec :: Spec
spec = do
  describe "Task1 benchmark" $ do
    it "(30, 100, 30)" $ do
      a <- generate $ getMatrix 30  100
      b <- generate $ getMatrix 100 30
      c <- generate $ getMatrix 30  30
      defaultMain
        [ bgroup "multiply naive"    [ bench "30x100 | 100x30" $ nf (multiplyNaive a) b ]
        , bgroup "multiply parallel" [ bench "30x100 | 100x30" $ nf (multiply      a) b ]
        , bgroup "multiply naive"    [ bench "30x30 | 30x100"  $ nf (multiplyNaive c) a ]
        , bgroup "multiply parallel" [ bench "30x30 | 30x100"  $ nf (multiply      c) a ]
        , bgroup "multiply naive"    [ bench "100x30 | 30x30"  $ nf (multiplyNaive b) c ]
        , bgroup "multiply parallel" [ bench "100x30 | 30x30"  $ nf (multiply      b) c ]
        ]
