{-# LANGUAGE FlexibleInstances #-}

module Task2
  ( HScript (..)
  , interpretScript
  ) where

import Data.Typeable (Typeable)

import Task1 (ST, STRef, newSTRef, readSTRef, runST, writeSTRef)

class (Typeable var, Ord var, Show var) => HVar var

instance HVar Int
instance HVar Double
instance HVar Bool
instance HVar String

class HScript script where
  (@=)      :: HVar a => STRef s a -> a -> script ()
  (@>)      :: HVar a => STRef s a -> a -> script Bool
  (@>=)     :: HVar a => STRef s a -> a -> script Bool
  (@<)      :: HVar a => STRef s a -> a -> script Bool
  (@<=)     :: HVar a => STRef s a -> a -> script Bool
  (@==)     :: HVar a => STRef s a -> a -> script Bool
  (#)       :: script a -> script b -> script b
  sWithVar1 :: HVar a => a -> (STRef s a -> script b) -> script b
  sWithVar2 :: (HVar a, HVar c)
            => a -> c -> (STRef s a -> STRef s c -> script b) -> script b
  sReadVar1 :: HVar a => STRef s a -> (a -> script b) -> script b
  sReadVar2 :: (HVar a, HVar c)
            => STRef s a -> STRef s c -> (a -> c -> script b) -> script b
  sLetVar   :: HVar a => a -> (a -> script b) -> script b
  sWhile    :: script Bool -> script a -> script ()
  sIf       :: script Bool -> script a -> script a -> script a
  sFun1     :: (HVar a, HVar b)
            => b -> (STRef s a -> STRef s b -> script d) -> a -> script b
  sFun2     :: (HVar a, HVar b, HVar c)
            => c -> (STRef s a -> STRef s b -> STRef s c -> script d) -> a -> b -> script c
  sReturn   :: HVar a => STRef s a -> script a

infixr 0 #
infixr 2 @=
infixr 2 @>
infixr 2 @>=
infixr 2 @<
infixr 2 @<=
infixr 2 @==

instance HScript (ST s) where
  ref @= val = writeSTRef ref val

  ref1 @> v2 = (> v2) <$> readSTRef ref1

  ref1 @>= v2 = (>= v2) <$> readSTRef ref1

  ref1 @< v2 = (< v2) <$> readSTRef ref1

  ref1 @<= v2 = (<= v2) <$> readSTRef ref1

  ref1 @== v2 = (== v2) <$> readSTRef ref1

  (#) = (>>)

  sWithVar1 val f = do
    ref <- newSTRef val
    f ref

  sWithVar2 val1 val2 f = do
    ref1 <- newSTRef val1
    ref2 <- newSTRef val2
    f ref1 ref2

  sReadVar1 ref f = do
    val <- readSTRef ref
    f val

  sReadVar2 ref1 ref2 f = do
    val1 <- readSTRef ref1
    val2 <- readSTRef ref2
    f val1 val2

  sLetVar val f = f val

  sWhile cond body = do
    isExec <- cond
    if (isExec)
     then do
       _ <- body
       sWhile cond body
     else pure ()

  sIf cond ifBody elseBody = do
    isExec <- cond
    if (isExec)
     then ifBody
     else elseBody

  sFun1 output script input = do
    input' <- newSTRef input
    output' <- newSTRef output
    _ <- script input' output'
    readSTRef output'

  sFun2 output script input1 input2 = do
    input1' <- newSTRef input1
    input2' <- newSTRef input2
    output' <- newSTRef output
    _ <- script input1' input2' output'
    readSTRef output'

  sReturn ref = do
    val <- readSTRef ref
    pure val

interpretScript :: ST s a -> a
interpretScript = runST
