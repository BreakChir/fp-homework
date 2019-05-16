{-# LANGUAGE BangPatterns #-}

module Task2
  ( Point (..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , absP
  , perimeter
  , doubleArea
  ) where

data Point = Point
  { x :: Int
  , y :: Int
  }

instance Show Point where
  show p = "(" ++ show (x p) ++ "," ++ show (y p) ++ ")"

plus :: Point -> Point -> Point
plus a b = Point (x a + x b) (y a + y b)

minus :: Point -> Point -> Point
minus a b = Point (x a - x b) (y a - y b)

scalarProduct :: Point -> Point -> Int
scalarProduct a b = x a * x b + y a * y b

crossProduct :: Point -> Point -> Int
crossProduct a b = x a * y b - y a * x b

absP :: Point -> Point -> Double
absP a b = (sqrt . fromIntegral) (dx * dx + dy * dy)
  where
    dx = x b - x a
    dy = y b - y a

perimeter :: [Point] -> Double
perimeter []     = 0
perimeter m@(c:_) = perimeterImpl m 0
  where
    perimeterImpl :: [Point] -> Double -> Double
    perimeterImpl []           _    = 0
    perimeterImpl [a]          !res = absP a c + res
    perimeterImpl (a:as@(b:_)) !res = perimeterImpl as (absP a b + res)

doubleArea :: [Point] -> Int
doubleArea []      = 0
doubleArea m@(c:_) = doubleAreaImpl m 0
  where
    doubleAreaImpl :: [Point] -> Int -> Int
    doubleAreaImpl []           _    = 0
    doubleAreaImpl [a]          !res = crossProduct a c + res
    doubleAreaImpl (a:as@(b:_)) !res = doubleAreaImpl as (crossProduct a b + res)
