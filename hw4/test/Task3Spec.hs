module Task3Spec
  ( spec
  ) where

import Task3 (gauss, verifySolution)

import Criterion.Main (bench, bgroup, defaultMain, nf)

import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, generate, vectorOf)

getBoolList :: Int -> Gen [Bool]
getBoolList size = do
  let genBool = choose (False, True)
  list <- vectorOf size genBool
  return list

getMatrix :: Int -> Int -> Gen [[Bool]]
getMatrix rows cols = do
  let genList = getBoolList cols
  list <- vectorOf rows genList
  return list

simpleTest :: Spec
simpleTest = do
  describe "Simple calculating tests" $ do
    it "has solution (rows = columns" $ do
      let a = [ [True, False, True , False]
              , [True, True , False, False]
              , [True, True , True , True ]
              , [True, True , True , True ]
              ]
      let y = [True, True, True, True]
      case gauss a y of
        Nothing   -> False `shouldBe` True
        Just ans' -> verifySolution a y ans' `shouldBe` True

    it "No solutions (rows > columns)" $ do
      let a = [ [True , False]
              , [True , True ]
              , [False, False]
              , [True , False]
              ]
      let y = [True, False, True, False]
      gauss a y `shouldBe` Nothing

    it "has solution (rows < columns)" $ do
      let a = [ [True, False, True , False]
              , [True, True , False, False]
              ]
      let y = [True, True]
      case gauss a y of
        Nothing   -> False `shouldBe` True
        Just ans' -> verifySolution a y ans' `shouldBe` True

randomTest :: Spec
randomTest = do
  it "benchmark on random matrix" $ do
    a100 <- generate $ getMatrix 100 100
    y100 <- generate $ getBoolList 100
    a200 <- generate $ getMatrix 200 200
    y200 <- generate $ getBoolList 200
    a300 <- generate $ getMatrix 300 300
    y300 <- generate $ getBoolList 300
    defaultMain
      [ bgroup "gauss" [ bench "100x100"   $ nf (gauss a100)  y100
                       , bench "200x200"   $ nf (gauss a200)  y200
                       , bench "300x300"   $ nf (gauss a300)  y300
                       ]
      ]

spec :: Spec
spec = do
  describe "Task3 tests" $ do
    simpleTest
    randomTest
