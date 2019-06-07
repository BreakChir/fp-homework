module Task2Spec
  ( spec
  ) where

import Task2 (HScript (..), interpretScript)

import Test.Hspec (Spec, describe, it, shouldBe)

log2 :: HScript script => Int -> script Int
log2 =
  sFun1 0 $ \a logCnt ->
  sWithVar1 0 $ \accum ->
    accum @= 1 #
    logCnt @= 0 #
    sWhile (sReadVar1 accum $ \eAccum -> a @> eAccum)
      ( sReadVar2 accum logCnt $ \eAccum eLogCnt ->
          accum @= eAccum + eAccum #
          logCnt @= eLogCnt + 1
      )

replicateStr :: HScript script => Int -> String -> script String
replicateStr =
  sFun2 "" $ \cnt str res ->
  sWhile (cnt @> 0)
    ( sReadVar2 str res $ \eStr eRes ->
      sReadVar1 cnt $ \eCnt ->
        res @= eRes ++ eStr #
        cnt @= eCnt - 1
    )

log2Test :: Spec
log2Test = do
  describe "log2" $ do
    it "log2 1" $
      interpretScript (log2 1) `shouldBe` 0
    it "log2 4" $
      interpretScript (log2 4) `shouldBe` 2
    it "log2 5" $
      interpretScript (log2 5) `shouldBe` 3
    it "log2 33" $
      interpretScript (log2 33) `shouldBe` 6

replicateTest :: Spec
replicateTest = do
  describe "log2" $ do
    it "replicateStr 0 \"bu\"" $
      interpretScript (replicateStr 0 "bu") `shouldBe` ""
    it "replicateStr 1 \"bu\"" $
      interpretScript (replicateStr 1 "bu") `shouldBe` "bu"
    it "replicateStr 3 \"bu\"" $
      interpretScript (replicateStr 3 "bu") `shouldBe` "bububu"
    it "replicateStr 4 \"bu\"" $
      interpretScript (replicateStr 4 "bu") `shouldBe` "bubububu"

spec :: Spec
spec =
  describe "Task2 Samples" $ do
    log2Test
    replicateTest
