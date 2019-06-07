{-# LANGUAGE BangPatterns #-}

module Task4Spec
  ( spec
  ) where

import Task2 (interpretScript)
import Task4 (fibHS, powHS, sqrtHS)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (choose, forAll, property)

powHSTest :: Spec
powHSTest = do
  describe "Test of pow function" $ do
    it "powHS 5 n" $
      forAll (choose (1, 100)) $ \n ->
        property (interpretScript (powHS 5 n) == (5 ^ n))
    it "powHS 2 n" $
      forAll (choose (1, 100)) $ \n ->
        property (interpretScript (powHS 2 n) == (2 ^ n))
    it "powHS 3 n" $
      forAll (choose (1, 100)) $ \n ->
        property (interpretScript (powHS 3 n) == (3 ^ n))

eps :: Double
eps = 1.0e-8

sqrtHSTest :: Spec
sqrtHSTest = do
  describe "Test of sqrt function" $ do
    it "sqrt n" $
      forAll (choose (1.0, 100000000.0)) $ \n ->
        property (interpretScript (sqrtHS n) - (sqrt n) < eps)

fibHaskell :: Int -> Int
fibHaskell n = go n (0, 1)
  where
    go :: Int -> (Int, Int) -> Int
    go !m (!a, !b)
      | m == 0    = a
      | otherwise = go (m - 1) (b, a + b)

fibHSTest :: Spec
fibHSTest = do
  describe "Test of fibonacci function" $ do
    it "fib n" $
      forAll (choose (1, 10000)) $ \n ->
        property (interpretScript (fibHS n) == fibHaskell n)

spec :: Spec
spec =
  describe "Task4 TESTS (math functions on HalyavaScript)" $ do
    powHSTest
    sqrtHSTest
    fibHSTest
