module Main
  ( main
  ) where

import qualified Task1Spec   as Task1Test
import qualified Task2Spec   as Task2Test
import qualified Task4Spec   as Task4Test
import qualified Task5Spec   as Task5Test
import qualified Task678Spec as LensFSTest

import           Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  Task1Test.spec
  Task2Test.spec
  Task4Test.spec
  Task5Test.spec
  LensFSTest.spec
