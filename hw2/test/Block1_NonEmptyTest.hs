module Block1_NonEmptyTest
  ( spec
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Monoid (Dual (..), Endo (..))
import Data.Functor.Compose (Compose (..))

import Block1 (NonEmpty (..))

import Test.Hspec (Spec, describe, it, shouldBe)

l4, b4 :: NonEmpty String
l4 = "4" :| []
b4 = "1245" :| ["124", "4222223", "2222224", "467", "1", "1222345"]

l3, b3 :: NonEmpty (a -> Maybe a)
l3 = Just :| []
b3 = Just :| [Just, Just, Just, Just, Just, Just, Just]

l2, b2 :: NonEmpty (String -> Int)
l2 = length :| []
b2 = length :| [length, length, length, length, length, length, length]

functorNonEmptyLaws :: Spec
functorNonEmptyLaws =
  describe "Functor NonEmpty" $ do
    describe "1. fmap id = id" $ do
      it "One element" $
        fmap id l4 `shouldBe` l4

      it "Many elements" $
        fmap id b4 `shouldBe` b4

    describe "2. fmap (f . g) = fmap f . fmap g" $ do
      it "One element" $
        fmap (Just . length) l4 `shouldBe` (fmap Just . fmap length) l4

      it "Many elements" $
        fmap (Just . length) b4 `shouldBe` (fmap Just . fmap length) b4

applicativeNonEmptyLaws :: Spec
applicativeNonEmptyLaws =
  describe "Applicative NonEmpty" $ do
    describe "1. pure id <*> v = v" $ do
      it "One element" $
        pure id <*> l4 `shouldBe` l4

      it "Many elements" $
        pure id <*> b4 `shouldBe` b4

    describe "2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      it "u = One element,   v = One element,   w = One element" $
        pure (.) <*> l3 <*> l2 <*> l4 `shouldBe` l3 <*> (l2 <*> l4)

      it "u = Many elements, v = One element,   w = Many elements" $
        pure (.) <*> b3 <*> l2 <*> b4 `shouldBe` b3 <*> (l2 <*> b4)

      it "u = Many elements, v = Many elements, w = Many elements" $
        pure (.) <*> b3 <*> b2 <*> b4 `shouldBe` b3 <*> (b2 <*> b4)

    describe "3. pure f <*> pure x = pure (f x)" $
      it "pure = One element" $
        (pure :: a -> NonEmpty a) Just <*> (pure :: a -> NonEmpty a) "4" `shouldBe`
        (pure :: a -> NonEmpty a) (Just "4")

    describe "4. u <*> pure y = pure ($ y) <*> u" $ do
      it "One element" $
        l3 <*> pure "4" `shouldBe` pure ($ "4") <*> l3

      it "Many elements" $
        b3 <*> pure "4" `shouldBe` pure ($ "4") <*> b3

foldableNonEmptyLaws :: Spec
foldableNonEmptyLaws =
  describe "Foldable NonEmpty" $ do
    describe "1. foldr f z t = appEndo (foldMap (Endo . f) t ) z" $ do
      it "One element" $
        foldr (++) "" l4 `shouldBe` appEndo (foldMap (Endo . (++)) l4 ) ""

      it "Many elements" $
        foldr (++) "" b4 `shouldBe` appEndo (foldMap (Endo . (++)) b4 ) ""

    describe "2. foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z" $ do
      it "One element" $
        foldl (++) "" l4 `shouldBe` appEndo (getDual (foldMap (Dual . Endo . flip (++)) l4)) ""

      it "Many elements" $
        foldl (++) "" b4 `shouldBe` appEndo (getDual (foldMap (Dual . Endo . flip (++)) b4)) ""

traversableNonEmptyLaws :: Spec
traversableNonEmptyLaws =
  describe "Traversable NonEmpty" $ do
    describe "1. t . traverse f = traverse (t . f)" $ do
      it "One element" $
        (maybeToEither . traverse Just) l4 `shouldBe` traverse (maybeToEither . Just) l4

      it "Many elements" $
        (maybeToEither . traverse Just) b4 `shouldBe` traverse (maybeToEither . Just) b4

    describe "2. traverse Identity = Identity" $ do
      it "One element" $
        traverse Identity l4 `shouldBe` Identity l4

      it "Many elements" $
        traverse Identity b4 `shouldBe` Identity b4

    describe "3. traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f" $ do
      it "One element" $
        traverse (Compose . fmap toEither . Just) l4 `shouldBe` (Compose . fmap (traverse toEither) . traverse Just) l4

      it "Many elements" $
        traverse (Compose . fmap toEither . Just) b4 `shouldBe` (Compose . fmap (traverse toEither) . traverse Just) b4
  where  
    maybeToEither :: Maybe a -> Either String a
    maybeToEither Nothing  = Left "Nothing"
    maybeToEither (Just x) = Right x 
    
    toEither :: String -> Either String String
    toEither s
      | length s < 3 = Left s
      | otherwise    = Right s

spec :: Spec
spec = do
  describe "checks some NonEmpty instances laws" $ do
    functorNonEmptyLaws
    applicativeNonEmptyLaws
    foldableNonEmptyLaws
    traversableNonEmptyLaws
