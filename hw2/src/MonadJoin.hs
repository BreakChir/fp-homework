{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadJoin
  ( MonadJoin
  , join
  , returnJoin
  ) where

class MonadJoin m where
  returnJoin :: a -> m a
  join       :: m (m a) -> m a
