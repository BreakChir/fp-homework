{-# LANGUAGE ExistentialQuantification #-}

module Task1
  ( ST
  , STRef (..)
  , newSTRef
  , modifySTRef
  , readSTRef
  , runST
  , writeSTRef
  ) where

import Control.Monad.State (State, evalState, get, modify, state)
import Data.Typeable (Typeable, cast)
import Data.List (find)

data STRef s a = STRef
  { refId :: Int
  }

data VarBox = forall a. Typeable a => VB Int a

data VarList = VarList
  { varCount :: Int
  , vars     :: [VarBox]
  }

type ST s = State VarList

initialState :: VarList
initialState = VarList 0 []

runST :: ST s a -> a
runST st = evalState st initialState

newSTRef :: Typeable a => a -> ST s (STRef s a)
newSTRef value = state addVar
  where
    addVar :: VarList -> (STRef s a, VarList)
    addVar vl = let ref = STRef (varCount vl)
                    vBox = VB (varCount vl) value
                 in (ref, VarList (varCount vl + 1) (vBox : vars vl))

readSTRef :: Typeable a => STRef s a -> ST s a
readSTRef ref = do
  vl <- get
  let vBox = find (\(VB vId _) -> vId == refId ref) (vars vl)
  case vBox of
    Just (VB _ x) -> case cast x of
                       Just x1 -> return x1
                       Nothing -> error "CAST IS FAILED"
    Nothing -> error "Read nonexistent ref"

modifySTRef :: Typeable a => STRef s a -> (a -> a) -> ST s ()
modifySTRef ref f = do
  modify modifyVarList
  where
    modifyVarList :: VarList -> VarList
    modifyVarList vl = vl { vars = map mapVar (vars vl) }

    mapVar :: VarBox -> VarBox
    mapVar vb@(VB vId val)
      | vId == refId ref = case cast val of
                             Just x1 -> VB vId (f x1)
                             Nothing -> error "CAST IS FAILED"
      | otherwise        = vb

writeSTRef :: Typeable a => STRef s a -> a -> ST s ()
writeSTRef ref value = modifySTRef ref (const value)
