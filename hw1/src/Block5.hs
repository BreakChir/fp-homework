module Block5
  ( Builder (..)
  , Endo (..)
  , Name (..)
  , ThisOrThat (..)
  , eitherConcat
  , fromString
  , maybeConcat
  , toString
  ) where

-- | Concats list of Maybe list
-- >>> maybeConcat [Nothing, Just [1,2], Nothing, Just [3,4,5,6,7,8,9]]
-- [1,2,3,4,5,6,7,8,9]
maybeConcat :: [Maybe [a]] -> [a]
maybeConcat list = 
  case mconcat list of
    Nothing -> []
    Just xs -> xs

-- | Concats list of Either
-- >>> eitherConcat [Left (Just [1,2]), Right [1,2,3], Left Nothing, Right [4,5]]
-- (Just [1,2],[1,2,3,4,5])
eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat = foldr innerConcat (mempty, mempty)
  where
    innerConcat :: (Monoid a, Monoid b) => Either a b -> (a, b) -> (a, b)
    innerConcat (Left l)  (ls, rs) = (l <> ls, rs)
    innerConcat (Right r) (ls, rs) = (ls, r <> rs)

data NonEmpty a = a :| [a]

instance Semigroup (NonEmpty a) where
  (x :| xs) <> (y :| ys) = x :| (xs ++ (y : ys))

data ThisOrThat a b
  = This a
  | That b
  | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (ThisOrThat a b) where
  This ax    <> This ay    = This (ax <> ay)
  This ax    <> That by    = Both ax by
  This ax    <> Both ay by = Both (ax <> ay) by  
  That bx    <> This ay    = Both ay bx
  That bx    <> That by    = That (bx <> by)
  That bx    <> Both ay by = Both ay (bx <> by)
  Both ax bx <> This ay    = Both (ax <> ay) bx
  Both ax bx <> That by    = Both ax (bx <> by)
  Both ax bx <> Both ay by = Both (ax <> ay) (bx <> by)

data Name
  = Name String
  | Empty

instance Semigroup Name where
  Name s <> Name t = Name (s ++ ('.' : t))
  a      <> Empty  = a
  Empty  <> a      = a

instance Monoid Name where
  mempty = Empty

newtype Endo a = Endo
  { getEndo :: a -> a
  }

instance Semigroup (Endo a) where
  Endo x <> Endo y = Endo (x . y)

instance Monoid (Endo a) where
  mempty = Endo id

data Builder
  = One Char
  | Many [Builder]
  deriving (Show)

instance Semigroup Builder where
  Many []    <> b          = b
  a          <> Many []    = a
  a@(One _)  <> b@(One _)  = Many [a, b]
  a@(One _)  <> Many list  = Many (a : list)
  Many list  <> b@(One _)  = Many (list ++ [b])
  Many list1 <> Many list2 = Many (list1 ++ list2)

instance Monoid Builder where
  mempty = Many []

-- | Converts String to Builder
-- >>> fromString "Put1N_V0DK."
-- Many [One 'P',One 'u',One 't',One '1',One 'N',One '_',One 'V',One '0',One 'D',One 'K',One '.']
fromString :: String -> Builder
fromString = foldMap One

-- | Converts Builder to String
-- >>> let builder1 = Many [One 'p', One 'i', Many [Many [Many [], One 'd'], Many [], One 'o'], One 'o']
-- >>> toString builder1
-- "pidoo"
toString :: Builder -> String
toString (One x)     = [x]
toString (Many list) = foldr (\x xs -> toString x ++ xs) [] list
