module Block1_SumTest
  ( spec
  ) where

import Block1 (stringSum)

import Test.Hspec (Spec, describe, it, shouldBe)

stringSumTest :: Spec
stringSumTest =
  describe "checks stringSum function" $ do
  it "3 137   5  4 1  " $
    stringSum "3 137   5  4 1  " `shouldBe` (Just 150)

  it "0 0 0   0 0" $
    stringSum "0 0 0   0 0" `shouldBe` (Just 0)

  it "0   0d 0   0 0" $
    stringSum "0   0d 0   0 0" `shouldBe` Nothing

  it "empty input" $
    stringSum "" `shouldBe` (Just 0)

  it "    3 23 -5 -1 26 0" $
    stringSum "    3 23 -5 -1 26 0" `shouldBe` (Just 46)

  it "0 fp2019" $
    stringSum "0 dh" `shouldBe` Nothing

spec :: Spec
spec = stringSumTest
