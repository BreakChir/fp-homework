module Main
  ( main
  ) where

import qualified Task1Spec  as Task1Test
import qualified Task2Spec  as Task2Test
import qualified Task3Spec  as Task3Test
import qualified Task4Spec  as Task4Test

import           Test.Hspec (hspec)
  
main :: IO ()
main = hspec $ do
  Task1Test.spec
  Task2Test.spec
  Task3Test.spec
  Task4Test.spec
