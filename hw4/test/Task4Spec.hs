module Task4Spec
  ( spec
  ) where

import Control.Concurrent (forkIO, threadDelay)

import Data.Hashable (Hashable(..))
import Data.Foldable (for_)

import Task4 (ConcurrentHashTable, getCHT, newCHT, putCHT)

import Test.Hspec (Spec, describe, it)

getCHTTest
  :: (Hashable k, Ord k, Show k, Show v)
  => Int
  -> k
  -> ConcurrentHashTable k v
  -> IO ()
getCHTTest thread k h = do
  val <- getCHT k h
  print $ (show thread) ++ " |GET| " ++ "k: " ++ (show k) ++ ", v: " ++ (show val)

putCHTTest
  :: (Hashable k, Ord k, Show k, Show v)
  => Int
  -> k
  -> v
  -> ConcurrentHashTable k v
  -> IO ()
putCHTTest thread k v h = do
  putCHT k v h
  print $ (show thread) ++ " |PUT| " ++ "k: " ++ (show k) ++ ", v: " ++ (show v)

spec :: Spec
spec = do
  describe "Task4 benchmark" $ do
    it "2 threads: [PUT,GET] 100 op/thread" $ do
      h <- newCHT :: IO (ConcurrentHashTable Int Int)
      let cntOp = [1..100]
      _threadId1 <- forkIO $ do
        for_ cntOp $ \i -> putCHTTest 1 i i h
        putStrLn "Forked thread 1 awake"
      _threadId2 <- forkIO $ do
        for_ cntOp $ \i -> getCHTTest 2 i h
        putStrLn "Forked thread 2 awake"
      threadDelay 1500000
      putStrLn "Main thread finishes"

    it "2 threads: [PUT,GET] 100 op/thread" $ do
      h <- newCHT :: IO (ConcurrentHashTable Int Int)
      let cntOp = [1..100]
      _threadId1 <- forkIO $ do
        for_ cntOp $ \i -> putCHTTest 1 i i h
        putStrLn "Forked thread 1 awake"
      _threadId2 <- forkIO $ do
        for_ cntOp $ \i -> getCHTTest 2 (i - 1) h
        putStrLn "Forked thread 2 awake"
      threadDelay 1500000
      putStrLn "Main thread finishes"