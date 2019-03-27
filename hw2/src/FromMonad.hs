{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans      #-}

module FromMonad
  (
  ) where

import Monad (Monad, (>>=), return)
import MonadFish (MonadFish, (>=>), returnFish)
import MonadJoin (MonadJoin, join, returnJoin)

import Prelude hiding (Monad, (>>=), return)

instance Monad m => MonadJoin m where
  returnJoin :: a -> m a
  returnJoin = return

  join :: m (m a) -> m a
  join x = x >>= id

instance Monad m => MonadFish m where
  returnFish :: a -> m a
  returnFish = return

  (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
  (>=>) f g x = f x >>= g
