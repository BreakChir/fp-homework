{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module MonadFish
  ( MonadFish
  , (>=>)
  , returnFish
  ) where

class MonadFish m where
  returnFish :: a -> m a
  (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)
