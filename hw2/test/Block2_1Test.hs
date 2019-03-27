module Block2_1Test
  ( spec
  ) where

import Block2_1 (ArithmeticError (..), BinOp (..), Expression (..), eval)

import Test.Hspec (Spec, describe, it, shouldBe)

correctExprSpec :: Spec
correctExprSpec =
  describe "checks correct Expressions" $ do
    it (show e1) $
      eval e1 `shouldBe` Right 30

    it (show e2) $
      eval e2 `shouldBe` Right 1024

    it (show e3) $
      eval e3 `shouldBe` Right (-994)

    it (show e4) $
      eval e4 `shouldBe` Right (-964)

    it (show e5) $
      eval e5 `shouldBe` Right (-11)

    it (show e6) $
      eval e6 `shouldBe` Right 1771561
  where
    e1 = Binary Mul (Const 5) (Const 6)
    e2 = Binary Pow (Const 2) (Const 10)
    e3 = Binary Sub e1 e2
    e4 = Binary Sum e3 e1
    e5 = Binary Div e4 (Binary Mul e1 (Const 3))
    e6 = Binary Pow e5 (Const 6)

incorrectExprSpec :: Spec
incorrectExprSpec =
  describe "checks incorrect Expressions" $ do
    it (show e3) $
      eval e3 `shouldBe` Left DivideByZero

    it (show e4) $
      eval e4 `shouldBe` Left DivideByZero

    it (show e5) $
      eval e5 `shouldBe` Left DivideByZero

    it (show e6) $
      eval e6 `shouldBe` Left NegatePow

    it (show e7) $
      eval e7 `shouldBe` Left NegatePow

    it (show e8) $
      eval e8 `shouldBe` Left NegatePow
  where
    e1 = Binary Mul (Const 5) (Const 6)
    e2 = Binary Sub (Const 2) (Const 10)
    e3 = Binary Div e2 (Binary Mul e1 (Const 0))
    e4 = Binary Mul e1 e3
    e5 = Binary Sub e3 e1
    e6 = Binary Pow (Const 3) e2
    e7 = Binary Sum e1 e6
    e8 = Binary Mul e6 e1

spec :: Spec
spec =
  describe "checks Expression eval" $ do
    correctExprSpec
    incorrectExprSpec
