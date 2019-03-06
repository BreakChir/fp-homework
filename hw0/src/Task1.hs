{-# LANGUAGE TypeOperators #-}

module Task1
  ( associator
  , distributivity
  , eitherAssoc
  ) where

distributivity
  :: Either a (b, c)
  -> (Either a b, Either a c)
distributivity (Left ap)        = (Left ap, Left ap)
distributivity (Right (bp, cp)) = (Right bp, Right cp)   

associator
  :: (a, (b, c))
  -> ((a, b), c)
associator (x, (y, z)) = ((x, y), z)

type (<->) a b = (a -> b, b -> a)

eitherAssoc
  :: Either a (Either b c)
  <-> Either (Either a b) c
eitherAssoc = (a1, a2)
  where 
    a1 :: (Either a (Either b c)) -> (Either (Either a b) c)
    a1 (Left x) = Left $ Left x
    a1 (Right (Left x)) = Left $ Right x
    a1 (Right (Right x)) = Right x
    
    a2 :: (Either (Either a b) c) -> (Either a (Either b c))
    a2 (Right x) = Right $ Right x
    a2 (Left (Left x)) = Left x
    a2 (Left (Right x)) = Right $ Left x
