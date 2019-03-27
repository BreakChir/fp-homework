module Main
  ( main
  ) where

import qualified Block1_SumTest      as Block1_SumTest
import qualified Block1_NonEmptyTest as Block1_NonEmptyTest
import qualified Block1_TreeTest     as Block1_TreeTest
import qualified Block2_1Test        as Block2_1Test
import qualified Block3_3Test        as Block3_3Test
import qualified Block3_4Test        as Block3_4Test

import Test.Hspec (hspec)
  
main :: IO ()
main = hspec $ do
  Block1_SumTest.spec
  Block1_TreeTest.spec
  Block1_NonEmptyTest.spec
  Block2_1Test.spec
  Block3_3Test.spec
  Block3_4Test.spec
