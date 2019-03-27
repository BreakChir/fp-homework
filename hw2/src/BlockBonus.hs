{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE TypeFamilies              #-}

module BlockBonus
  ( Cont (..)
  , ExitStatus (..)
  , Paused (..)
  , exit
  , fork
  , kernel
  , main'
  , read
  , write
  , yield
  ) where

import Prelude hiding (read)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, modify, put)

newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f contA = Cont $ \br -> runCont contA (br . f)

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure a = Cont ($ a)

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  Cont abrr <*> Cont arr = Cont $ \br -> abrr $ \ab -> arr $ \a -> br (ab a)

instance Monad (Cont r) where
  return :: a -> Cont r a
  return a = Cont ($ a)

  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  Cont arr >>= f = Cont $ \br -> arr $ \a -> runCont (f a) br

data ExitStatus
  = Success
  | Failure
  deriving (Show)

data ReadTag  = ReadTag
data WriteTag = WriteTag
data YieldTag = YieldTag
data ExitTag  = ExitTag
data ForkTag  = ForkTag

class Syscall sTag where
  type SyscallArg sTag :: *
  type SyscallRes sTag :: *

  doAction :: sTag -> SyscallArg sTag -> (SyscallRes sTag -> Paused) -> ProcessState ()

instance Syscall ReadTag where
  type SyscallArg ReadTag = ()
  type SyscallRes ReadTag = String

  doAction :: ReadTag -> () -> (String -> Paused) -> ProcessState ()
  doAction _ _ conts' = do
    s <- lift getLine
    addProcess (Process conts' s)

instance Syscall WriteTag where
  type SyscallArg WriteTag = String
  type SyscallRes WriteTag = ()

  doAction :: WriteTag -> String -> (() -> Paused) -> ProcessState ()
  doAction _ s conts' = do
    lift $ putStrLn s
    addProcess (Process conts' ())

instance Syscall YieldTag where
  type SyscallArg YieldTag = ()
  type SyscallRes YieldTag = ()

  doAction :: YieldTag -> () -> (() -> Paused) -> ProcessState ()
  doAction _ _ conts' = addProcess (Process conts' ())

instance Syscall ExitTag where
  type SyscallArg ExitTag = ExitStatus
  type SyscallRes ExitTag = ()

  doAction :: ExitTag -> ExitStatus -> (() -> Paused) -> ProcessState ()
  doAction _ st _ = lift $ putStrLn ("Exit " ++ (show st))

instance Syscall ForkTag where
  type SyscallArg ForkTag = ()
  type SyscallRes ForkTag = ()

  doAction :: ForkTag -> () -> (() -> Paused) -> ProcessState ()
  doAction _ _ conts' = do 
    addProcess (Process conts' ())
    addProcess (Process conts' ())

data Paused = forall sTag . Syscall sTag => Paused
  { tag   :: sTag
  , arg   :: SyscallArg sTag
  , conts :: (SyscallRes sTag) -> Paused
  }

syscall :: Syscall sTag => sTag -> SyscallArg sTag -> Cont Paused (SyscallRes sTag)
syscall tag' arg' = Cont $ \cont' -> Paused tag' arg' cont'

read :: Cont Paused String
read = syscall ReadTag ()

write :: String -> Cont Paused ()
write = syscall WriteTag

yield :: Cont Paused ()
yield = syscall YieldTag ()

exit :: ExitStatus -> Cont Paused ()
exit = syscall ExitTag

fork :: Cont Paused ()
fork = syscall ForkTag ()

main' :: Cont Paused ()
main' = do
  x <- read
  let str = "Hello, " ++ show x ++ "!"
  write str
  exit Success

data Process = forall r . Process (r -> Paused) r

type ProcessState = StateT [Process] IO

addProcess :: Process -> ProcessState ()
addProcess p = modify $ \list -> list ++ [p]

kernel :: Cont Paused () -> IO ()
kernel startValue = evalStateT kernel' [Process (runCont startValue) undefined]
  where
    kernel' :: ProcessState ()
    kernel' = do
      processes <- get
      case processes of
        [] -> return ()
        ((Process contRun' argRun'):prs) -> do
          put prs
          case contRun' argRun' of
            Paused tag' arg' conts' -> do
              doAction tag' arg' conts'
              kernel'
