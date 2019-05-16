module Task2Spec
  ( spec
  ) where

import Task2 (Point (..), absP, crossProduct, doubleArea, perimeter)

import Criterion.Main (bench, bgroup, defaultMain, nf)

import Test.Hspec (Spec, describe, it)
import Test.QuickCheck (Gen, choose, generate, vectorOf)

perimeterNaive :: [Point] -> Double
perimeterNaive []     = 0
perimeterNaive m@(c:_) = perimeterImpl m 0
  where
    perimeterImpl :: [Point] -> Double -> Double
    perimeterImpl []           _    = 0
    perimeterImpl [a]          res = absP a c + res
    perimeterImpl (a:as@(b:_)) res = perimeterImpl as (absP a b + res)

doubleAreaNaive :: [Point] -> Int
doubleAreaNaive []      = 0
doubleAreaNaive m@(c:_) = doubleAreaImpl m 0
  where
    doubleAreaImpl :: [Point] -> Int -> Int
    doubleAreaImpl []           _    = 0
    doubleAreaImpl [a]          res = crossProduct a c + res
    doubleAreaImpl (a:as@(b:_)) res = doubleAreaImpl as (crossProduct a b + res)

getGenPoint :: Gen Point
getGenPoint = do
  xP <- choose (-10000000, 1000000000)
  yP <- choose (-10000000, 1000000000)
  return (Point xP yP)

getPointList :: Int -> Gen [Point]
getPointList size = do
  let genPoint = getGenPoint
  list <- vectorOf size genPoint
  return list

spec :: Spec
spec =
  describe "Task2 benchmark" $ do
    it "10^3 points" $ do
      list3 <- generate $ getPointList 1000
      defaultMain
        [ bgroup "perimeter naive"  [ bench "10^3 points" $ nf perimeterNaive  list3 ]
        , bgroup "perimeter"        [ bench "10^3 points" $ nf perimeter       list3 ]
        , bgroup "doubleArea naive" [ bench "10^3 points" $ nf doubleAreaNaive list3 ]
        , bgroup "doubleArea"       [ bench "10^3 points" $ nf doubleArea      list3 ]
        ]

    it "10^5 points" $ do
      list5 <- generate $ getPointList 100000
      defaultMain
        [ bgroup "perimeter naive"  [ bench "10^5 points" $ nf perimeterNaive  list5 ]
        , bgroup "perimeter"        [ bench "10^5 points" $ nf perimeter       list5 ]
        , bgroup "doubleArea naive" [ bench "10^5 points" $ nf doubleAreaNaive list5 ]
        , bgroup "doubleArea"       [ bench "10^5 points" $ nf doubleArea      list5 ]
        ]

    it "10^7 points" $ do
      list7 <- generate $ getPointList 10000000
      defaultMain
        [ bgroup "perimeter naive"  [ bench "10^7 points" $ nf perimeterNaive  list7 ]
        , bgroup "perimeter"        [ bench "10^7 points" $ nf perimeter       list7 ]
        , bgroup "doubleArea naive" [ bench "10^7 points" $ nf doubleAreaNaive list7 ]
        , bgroup "doubleArea"       [ bench "10^7 points" $ nf doubleArea      list7 ]
        ]
