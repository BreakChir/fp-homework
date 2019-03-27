module Block3_4Test
  ( spec
  ) where

import Block3_1 (runParser)
import Block3_4 (intLists)

import Test.Hspec (Spec, describe, it, shouldBe)

intListsSpec :: Spec
intListsSpec =
  describe "checks int lists parser" $ do
    it "[ [1, 2, 1] ]" $
      runParser intLists "003, 1, 2, 01    "
        `shouldBe` Just([ [1, 2, 1] ], "")
    it "[ [1, 10], [5, -7, 2] ]" $
      runParser intLists "2, 1,+10  , 3,5,-7, 2"
        `shouldBe` Just([ [1, 10], [5, -7, 2] ], "")
    it "len is zero" $
      runParser intLists "    2, 1,    +10  , 3  ,  5,-7,   2 ,  0  "
        `shouldBe` Just([ [1, 10], [5, -7, 2], [] ], "")
    it "negative len" $
      runParser intLists "-2, 1,    +10  , 0  "
        `shouldBe` Nothing
    it "negative len without elements" $
      runParser intLists "-2"
        `shouldBe` Nothing
    it "doesn't enough ints" $
      runParser intLists "2, 1,   " `shouldBe` Nothing
    it "empty list" $
      runParser intLists "" `shouldBe` Just([], "")

spec :: Spec
spec =
  describe "checks hard parser" $ do
    intListsSpec
