module Block3_3
  ( Nat (..)
  , fromNat
  , natCompare
  , natDiv
  , natEq
  , natEven
  , natMod
  , natMul
  , natPlus
  , natSub
  , toNat
  ) where

data Nat
  = Z
  | S Nat
  deriving (Show)

-- | Sums two Nats
-- >>> natPlus Z (S (S Z))
-- S (S Z)
-- >>> natPlus (S (S Z)) (S (S Z))
-- S (S (S (S Z)))
natPlus :: Nat -> Nat -> Nat
natPlus a Z     = a
natPlus a (S k) = S (natPlus a k)

-- | Multiples two Nats
-- >>> natMul Z (S (S Z))
-- Z
-- >>> natMul (S (S (S Z))) (S (S Z))
-- S (S (S (S (S (S Z)))))
natMul :: Nat -> Nat -> Nat
natMul _ Z     = Z
natMul a (S k) = natPlus a (natMul a k)

-- | Subtracts two Nats
-- >>> natSub Z (S (S Z))
-- Z
-- >>> natSub (S (S (S Z))) (S (S Z))
-- S Z
natSub :: Nat -> Nat -> Nat
natSub a     Z     = a
natSub Z     _     = Z
natSub (S m) (S k) = natSub m k

-- | Сonverts Int to Nat
-- >>> toNat 2
-- S (S Z)
-- >>> toNat 6
-- S (S (S (S (S (S Z)))))
toNat :: Int -> Nat
toNat 0 = Z
toNat n
  | n < 0     = error "Expected Non negative number"
  | otherwise = S (toNat (n - 1))

-- | Сonverts Nat to Int
-- >>> fromNat (S (S Z))
-- 2
-- >>> fromNat (S (S (S (S (S (S Z))))))
-- 6
fromNat :: Nat -> Int
fromNat Z     = 0
fromNat (S k) = 1 + fromNat k

-- | Check if two Nats are equal
-- >>> natEq (S (S Z)) (S Z)
-- False
-- >>> natEq (S (S Z)) (S (S Z))
-- True
natEq :: Nat -> Nat -> Bool
natEq (S m) (S k) = natEq m k
natEq Z     Z     = True
natEq _     _     = False

-- | Compares two Nats
-- >>> natCompare (S (S Z)) (S Z)
-- GT
-- >>> natCompare (S (S Z)) (S (S Z))
-- EQ
-- >>> natCompare (S Z) (S (S Z))
-- LT
natCompare :: Nat -> Nat -> Ordering
natCompare (S m) (S k) = natCompare m k
natCompare Z     Z     = EQ
natCompare Z     _     = LT
natCompare _     _     = GT

-- | Check if Nat is even
-- >>> natEven (S (S Z))
-- True
-- >>> natEven (S (S (S Z)))
-- False
natEven :: Nat -> Bool
natEven Z     = True
natEven (S k) = not (natEven k) 

-- | Divides first Nat by second
-- >>> natDiv (S (S (S Z))) (S (S Z))
-- S Z
-- >>> natDiv (S (S (S (S (S (S Z)))))) (S (S (S Z)))
-- S (S Z)
natDiv :: Nat -> Nat -> Nat
natDiv _ Z = error "Second argument is zero"
natDiv n m = case (natCompare n m) of
               EQ -> S Z
               LT -> Z
               GT -> S (natDiv (natSub n m) m)

-- | Get remainder
-- >>> natMod (S (S (S Z))) (S (S Z))
-- S Z
-- >>> natMod (S (S (S (S (S (S Z)))))) (S (S (S Z)))
-- Z
natMod :: Nat -> Nat -> Nat
natMod _ Z = error "Second argument is zero"
natMod n m = case (natCompare n m) of
               EQ -> Z
               LT -> n
               GT -> natMod (natSub n m) m
