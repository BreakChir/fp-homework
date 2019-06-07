module Task5Spec
  ( spec
  ) where

import Task5 ((^.), _1, _2, over, set)

import Test.Hspec (Spec, describe, it, shouldBe)

a :: (String, Int)
a = ("I'm prosharil lens", 23)

pairLensLawsTest :: Spec
pairLensLawsTest = do
  describe "checks laws for pair lens: _1" $ do
    it "view l (set l field obj) = field" $
      ((set _1 "Or not prosharil" a) ^. _1) `shouldBe` "Or not prosharil"
    it "set l (view l obj) obj = obj" $
      (set _1 (a ^. _1) a) `shouldBe` a
    it "set l field (set l field obj) = set l field obj" $
      (set _1 "Privet" (set _1 "Privet" a)) `shouldBe` (set _1 "Privet" a)

  describe "checks laws for pair lens: _2" $ do
    it "view l (set l field obj) = field" $
      ((set _2 13 a) ^. _2) `shouldBe` 13
    it "set l (view l obj) obj = obj" $
      (set _2 (a ^. _2) a) `shouldBe` a
    it "set l field (set l field obj) = set l field obj" $
      (set _2 13 (set _2 13 a)) `shouldBe` (set _2 13 a)

pairLensTest :: Spec
pairLensTest = do
  describe "checks set, view, over for _1" $ do
    it "checks over/view" $
      ((over _1 (++ "!!!") a) ^. _1) `shouldBe` "I'm prosharil lens!!!"
    it "checks set/view" $
      ((set _1 "!!!" a) ^. _1) `shouldBe` "!!!"
  describe "checks set, view, over for _2" $ do
    it "checks over/view" $
      ((over _2 (+ 23) a) ^. _2) `shouldBe` 46
    it "checks set/view" $
      ((set _2 12 a) ^. _2) `shouldBe` 12

spec :: Spec
spec =
  describe "Task5 TESTS" $ do
    pairLensLawsTest
    pairLensTest
