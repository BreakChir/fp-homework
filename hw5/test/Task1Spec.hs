module Task1Spec
  ( spec
  ) where

import Data.Foldable (for_, forM_)

import Task1 (newSTRef, modifySTRef, readSTRef, runST, writeSTRef)

import Test.Hspec (Spec, describe, it, shouldBe)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib i = runST $ do
  s <- newSTRef 0
  t <- newSTRef 1
  forM_ [2..i] $ \_ -> do
    _s <- readSTRef s
    _t <- readSTRef t
    writeSTRef s _t
    writeSTRef t (_s + _t)
  readSTRef t

eps :: Double
eps = 1.0e-15

whileM :: Monad m => m Bool -> m () -> m ()
whileM c act =
  c >>= \b ->
    if b
      then act >> whileM c act
      else pure ()

sqrt' :: Double -> Double
sqrt' x
  | x < 1 = error "x < 1 not supported"
  | otherwise = runST $ do
      l <- newSTRef 0
      r <- newSTRef x
      let checkCond = do
           l_ <- readSTRef l
           r_ <- readSTRef r
           pure (r_ - l_ > eps)
      whileM checkCond $ do
        l_ <- readSTRef l -- l^2 < x
        r_ <- readSTRef r -- r^2 >= x
        let m = (l_ + r_) / 2
        if (m * m >= x)
          then writeSTRef r m
          else writeSTRef l m
      readSTRef r

pow' :: Int -> Int -> Int
pow' a' n'
  | n' < 0    = error "n < 1 not supported"
  | n' == 0   = 1
  | otherwise = runST $ do
      res <- newSTRef 1
      n <- newSTRef n'
      a <- newSTRef a'
      let checkCond = do
           n_ <- readSTRef n
           pure (n_ > 0)
      whileM checkCond $ do
        n_ <- readSTRef n
        a_ <- readSTRef a
        if (n_ `mod` 2 == 1)
         then modifySTRef res (* a_)
         else pure ()
        modifySTRef a (* a_)
        modifySTRef n (`div` 2)
      readSTRef res

fibTest :: Spec
fibTest = do
  describe "Test of Fibonacci number function" $ do
    let fibAns = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610,
                 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368,
                 75025, 121393, 196418, 317811]
    for_ (zip [0..28] fibAns) $ \(i, ans) ->
      it ("fib " <> (show i)) $
        fib i `shouldBe` ans

sqrtTest :: Spec
sqrtTest = do
  describe "Test of sqrt function" $ do
    it "sqrt' 16" $
      putStrLn . show . sqrt' $ 16
    it "sqrt' 18" $
      putStrLn . show . sqrt' $ 18
    it "sqrt' 59" $
      putStrLn . show . sqrt' $ 59
    it "sqrt' 27" $
      putStrLn . show . sqrt' $ 27

powTest :: Spec
powTest = do
  describe "Test of pow function" $ do
    it "pow' 2 5" $
      pow' 2 5 `shouldBe` 32
    it "pow' 3 12" $
      pow' (-3) 11 `shouldBe` -177147
    it "pow' 5 5" $
      pow' 5 5 `shouldBe` 3125
    it "pow' 27" $
      pow' (-2) 12 `shouldBe` 4096

spec :: Spec
spec =
  describe "Task1 TESTS (Analog of ST Monad)" $ do
    fibTest
    sqrtTest
    powTest
