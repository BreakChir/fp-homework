{-# LANGUAGE RankNTypes #-}

module Task5
  ( Lens'
  , (<%~)
  , (<<%~)
  , (.~)
  , (^.)
  , (%~)
  , _1
  , _2
  , choosing
  , lens
  , over
  , set
  , view
  ) where

import Control.Monad (join)

import Data.Functor.Const (Const (..))
import Data.Functor.Identity (Identity (..))

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t

type Lens' s a  = Lens s s a a

set  :: Lens' s a -> a -> s -> s
set l a s = runIdentity $ l (Identity . const a) s

view :: Lens' s a -> s -> a
view l s = getConst $ l Const s

over :: Lens' s a -> (a -> a) -> s -> s
over l fn s = runIdentity $ l (Identity . fn) s

(.~) :: Lens' s a -> a -> s -> s
(.~) = set

(^.) :: s -> Lens' s a -> a
(^.) s l = view l s

(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) = over

_1 :: Lens (a, x) (b, x) a b
_1 f (a, x) = (flip (,) x) <$> f a

_2 :: Lens (x, a) (x, b) a b
_2 f (x, a) = (,) x <$> f a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter = \f s -> setter s <$> f (getter s)

choosing :: Lens s1 t1 a b
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 _  f (Left  s) = Left  <$> l1 f s
choosing _  l2 f (Right s) = Right <$> l2 f s

(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (join (,) . f) s

(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (fmap f . join (,)) s
