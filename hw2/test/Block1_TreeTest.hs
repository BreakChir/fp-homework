module Block1_TreeTest
  ( spec
  ) where

import Control.Monad.Identity (Identity (..))
import Data.Monoid (Dual (..), Endo (..))
import Data.Functor.Compose (Compose (..))

import Block1 (Tree (..))

import Test.Hspec (Spec, describe, it, shouldBe)

a4, l4, b4 :: Tree String
a4 = Leaf "12345"
l4 = Leaf "4"
b4 = Branch (Branch a4 (Branch l4 l4)) (Branch l4 (Branch a4 l4))

l3, b3 :: Tree (a -> Maybe a)
l3 = Leaf Just
b3 = Branch (Branch l3 l3) (Branch l3 (Branch l3 l3))

l2, b2 :: Tree (String -> Int)
l2 = Leaf length
b2 = Branch (Branch l2 (Branch l2 (Branch (Branch l2 l2) l2))) l2

functorTreeLaws :: Spec
functorTreeLaws =
  describe "Functor Tree" $ do
    describe "1. fmap id = id" $ do
      it "Leaf" $
        fmap id l4 `shouldBe` l4

      it "Branch" $
        fmap id b4 `shouldBe` b4

    describe "2. fmap (f . g) = fmap f . fmap g" $ do
      it "Leaf" $
        fmap (Just . length) l4 `shouldBe` (fmap Just . fmap length) l4

      it "Branch" $
        fmap (Just . length) b4 `shouldBe` (fmap Just . fmap length) b4

applicativeTreeLaws :: Spec
applicativeTreeLaws =
  describe "Applicative Tree" $ do
    describe "1. pure id <*> v = v" $ do
      it "Leaf" $
        pure id <*> l4 `shouldBe` l4

      it "Branch" $
        pure id <*> b4 `shouldBe` b4

    describe "2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)" $ do
      it "u = Leaf,   v = Leaf,   w = Leaf" $
        pure (.) <*> l3 <*> l2 <*> l4 `shouldBe` l3 <*> (l2 <*> l4)

      it "u = Branch, v = Leaf,   w = Branch" $
        pure (.) <*> b3 <*> l2 <*> b4 `shouldBe` b3 <*> (l2 <*> b4)

      it "u = Branch, v = Branch, w = Branch" $
        pure (.) <*> b3 <*> b2 <*> b4 `shouldBe` b3 <*> (b2 <*> b4)

    describe "3. pure f <*> pure x = pure (f x)" $
      it "pure = Leaf" $
        (pure :: a -> Tree a) Just <*> (pure :: a -> Tree a) "4" `shouldBe`
        (pure :: a -> Tree a) (Just "4")

    describe "4. u <*> pure y = pure ($ y) <*> u" $ do
      it "Leaf" $
        l3 <*> pure "4" `shouldBe` pure ($ "4") <*> l3

      it "Branch" $
        b3 <*> pure "4" `shouldBe` pure ($ "4") <*> b3

foldableTreeLaws :: Spec
foldableTreeLaws =
  describe "Foldable Tree" $ do
    describe "1. foldr f z t = appEndo (foldMap (Endo . f) t ) z" $ do
      it "Leaf" $
        foldr (++) "" l4 `shouldBe` appEndo (foldMap (Endo . (++)) l4 ) ""

      it "Branch" $
        foldr (++) "" b4 `shouldBe` appEndo (foldMap (Endo . (++)) b4 ) ""

    describe "2. foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z" $ do
      it "Leaf" $
        foldl (++) "" l4 `shouldBe` appEndo (getDual (foldMap (Dual . Endo . flip (++)) l4)) ""

      it "Branch" $
        foldl (++) "" b4 `shouldBe` appEndo (getDual (foldMap (Dual . Endo . flip (++)) b4)) ""

traversableTreeLaws :: Spec
traversableTreeLaws =
  describe "Traversable Tree" $ do
    describe "1. t . traverse f = traverse (t . f)" $ do
      it "Leaf" $
        (maybeToEither . traverse Just) l4 `shouldBe` traverse (maybeToEither . Just) l4

      it "Branch" $
        (maybeToEither . traverse Just) b4 `shouldBe` traverse (maybeToEither . Just) b4

    describe "2. traverse Identity = Identity" $ do
      it "Leaf" $
        traverse Identity l4 `shouldBe` Identity l4

      it "Branch" $
        traverse Identity b4 `shouldBe` Identity b4

    describe "3. traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f" $ do
      it "Leaf" $
        traverse (Compose . fmap toEither . Just) l4 `shouldBe` (Compose . fmap (traverse toEither) . traverse Just) l4

      it "Branch" $
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
  describe "checks some Tree instances laws" $ do
    functorTreeLaws
    applicativeTreeLaws
    foldableTreeLaws
    traversableTreeLaws
