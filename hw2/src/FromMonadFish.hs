{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module FromMonadFish
  (
  ) where

import Monad (Monad, (>>=), return)
import MonadFish (MonadFish, (>=>), returnFish)
import MonadJoin (MonadJoin, join, returnJoin)

import Prelude hiding (Monad, return, (>>=))

instance MonadFish m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = returnFish

  join :: m (m a) -> m a
  join = (>=>) id id

instance MonadFish m => Monad m where
  return :: a -> m a
  return = returnFish

  (>>=) :: m a -> (a -> m b) -> m b
  x >>= f = (>=>) id f x
