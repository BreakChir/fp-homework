module Task2
  ( doubleNeg
  , doubleNegElim
  , excludedNeg
  , pierce
  , thirdNegElim
  ) where

import Data.Void (Void)

type Neg a = a -> Void

doubleNeg :: a -> Neg (Neg a)
doubleNeg ap aToVoid = aToVoid ap

excludedNeg :: Neg (Neg (Either a (Neg a)))
excludedNeg = axiom9 fstMP sndMP
  where
    axiom9 :: (a -> b) -> (a -> Neg b) -> Neg a
    axiom9 aToB aToBToVoid a = aToBToVoid a $ aToB a

    contraposition :: (a -> b) -> Neg b -> Neg a
    contraposition aToB bToVoid a = bToVoid $ aToB a

    fstMP = contraposition Left
    sndMP = contraposition Right

-- it doesn't have term
pierce :: ((a -> b) -> a) -> a
pierce = undefined

-- it doesn't have term
doubleNegElim :: Neg (Neg a) -> a
doubleNegElim = undefined

thirdNegElim :: Neg (Neg (Neg a)) -> Neg a
thirdNegElim f a = f $ doubleNeg a
