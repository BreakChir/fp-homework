{-# LANGUAGE InstanceSigs #-}

module Block2_1
  ( ArithmeticError (..)
  , BinOp (..)
  , Expression (..)
  , eval
  ) where

data BinOp
  = Sum
  | Sub
  | Mul
  | Div
  | Pow

instance Show BinOp where
  show :: BinOp -> String
  show Sum = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Pow = " ^ "

data Expression
  = Const Int
  | Binary BinOp Expression Expression

instance Show Expression where
  show :: Expression -> String
  show (Binary op a b) = "(" ++ show a ++ show op ++ show b ++ ")"
  show (Const a)       = show a

data ArithmeticError
  = DivideByZero
  | NegatePow
  deriving (Show)

instance Eq ArithmeticError where
  (==) :: ArithmeticError -> ArithmeticError -> Bool
  DivideByZero == DivideByZero = True
  NegatePow    == NegatePow    = True
  _            == _            = False

eval :: Expression -> Either ArithmeticError Int
eval (Const a)       = Right a
eval (Binary op a b) = (toIntOp op <$> eval a <*> eval b) >>= id
  where
    toIntOp :: BinOp -> Int -> Int -> Either ArithmeticError Int
    toIntOp Sum x y = Right (x + y)
    toIntOp Sub x y = Right (x - y)
    toIntOp Mul x y = Right (x * y)
    toIntOp Div x y
      | y == 0      = Left DivideByZero
      | otherwise   = Right (x `div` y)
    toIntOp Pow x y
      | y < 0       = Left NegatePow
      | otherwise   = Right (x ^ y)
