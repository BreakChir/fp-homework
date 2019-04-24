module Structure
  ( Command (..)
  , Ctx (..)
  , CtxIO
  , Parser
  , TemplateArg (..)
  , Value (..)
  ) where

import           Control.Monad.Reader (ReaderT (..))

import           Data.IORef (IORef)
import qualified Data.Map  as M
import qualified Data.Text as T
import           Data.Void (Void)

import           Text.Megaparsec (Parsec)

type Parser = Parsec Void T.Text

data TemplateArg
  = TemplateVar T.Text
  | TemplateText T.Text
  | TemplateShell [Command]

data Value
  = Template [TemplateArg]
  | Variable T.Text
  | Text T.Text
  | SubShell [Command]

type NewLine = Bool

data Command
  = Assignment T.Text [Value]
  | Echo NewLine [[Value]]
  | Exit T.Text
  | PWD
  | CD [Value]
  | Read [T.Text]
  | IfThen [Command] [Command] [Command]
  | ReadReturnCode [Command] [Command]
  | While [Command] [Command]
  | WhileReadCode [Command] [Command]
  | SubShellCommand [Command]
  | Unknown [Value] [[Value]]

data Ctx = Ctx
  { gcVars       :: IORef (M.Map T.Text T.Text)
  , gcCommands   :: IORef [Command]
  , gcReturnCode :: IORef Int
  , gcOutput     :: IORef String
  , gcPath       :: IORef FilePath
  , isSubShell   :: Bool
  }

type CtxIO a = ReaderT Ctx IO a
