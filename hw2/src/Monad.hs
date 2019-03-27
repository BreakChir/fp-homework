{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Monad
  ( Monad
  , (>>=)
  , return
  ) where

import Prelude hiding (Monad, (>>=), return)

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
