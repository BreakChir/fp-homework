module Block3_3Test
  ( spec
  ) where

import Data.Maybe (isJust, isNothing)

import Block3_1 (runParser)
import Block3_3 (bracketSequence, int)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

intSpec :: Spec
intSpec =
  describe "checks int parsers" $ do
    describe "when input is correct" $ do
      it "126" $
        runParser int "126" `shouldBe` Just (126, "")
      it "-0000126443" $
        runParser int "-0000126443" `shouldBe` Just (-126443, "")
      it "+0126" $
        runParser int "+126" `shouldBe` Just (126, "")

    describe "when input is incorrect" $ do
      it "++126" $
        runParser int "++126" `shouldBe` Nothing
      it "+" $
        runParser int "+" `shouldBe` Nothing
      it "-" $
        runParser int "-" `shouldBe` Nothing
      it "cat123" $
        runParser int "cat123" `shouldBe` Nothing

bracketSpec :: Spec
bracketSpec =
  describe "checks bracketSequence parser" $ do
    it "(())" $
      runParser bracketSequence "(())" `shouldSatisfy` isJust

    it "(()))(" $
      runParser bracketSequence "(()))(" `shouldSatisfy` isNothing

    it "(()))" $
      runParser bracketSequence "(()))" `shouldSatisfy` isNothing

    it "((())" $
      runParser bracketSequence "((())" `shouldSatisfy` isNothing

    it "((()))kekRzshakaLookToKonca" $
      runParser bracketSequence "((()))kekRzshakaLookToKonca" `shouldSatisfy` isNothing

spec :: Spec
spec =
  describe "checks simple parsers" $ do
    bracketSpec
    intSpec
