{-# LANGUAGE LambdaCase #-}

module Task1
  ( interpret
  ) where

import           Control.Monad.Reader (MonadReader (..), ReaderT (..))
import           Control.Monad.IO.Class (liftIO)

import           Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map  as M
import qualified Data.Text as T

import           System.Directory (doesFileExist, doesDirectoryExist, getCurrentDirectory)
import           System.Environment (getArgs)
import           System.Exit (ExitCode (..))
import           System.FilePath ((</>), takeDirectory, splitPath)
import           System.IO (hGetContents)
import           System.Process (StdStream (..), CreateProcess(..), createProcess, proc, waitForProcess)

import           SimpleBashParser (parseCommand, parseScript)
import           Structure (Command (..), Ctx (..), CtxIO, TemplateArg (..), Value (..))

setArgs :: [String] -> String -> IO (IORef (M.Map T.Text T.Text))
setArgs args script = newIORef $ M.fromList $ setArgs' 1 args
  where
    setArgs' :: Int -> [String] -> [(T.Text, T.Text)]
    setArgs' _ []     = [(T.pack "0", T.pack script)]
    setArgs' n (x:xs) = (T.pack (show n), T.pack x) : (setArgs' (n + 1) xs)

runSubShell :: [Command] -> CtxIO String
runSubShell cmnds = do
  ctx <- ask
  cmndsRef <- liftIO $ newIORef cmnds
  returnedCode <- liftIO $ newIORef 0
  outputRef <- liftIO $ newIORef ""
  let ctx' = Ctx (gcVars ctx) cmndsRef returnedCode outputRef (gcPath ctx) True
  out <- liftIO $ runReaderT runScript ctx'
  case out of
    [] -> return []
    _  -> return (init out)

concatTemplateText :: (M.Map T.Text T.Text) -> [TemplateArg] -> T.Text -> CtxIO T.Text
concatTemplateText _    []     z = return z
concatTemplateText vars (x:xs) z =
  case x of
    TemplateText  t -> concatTemplateText vars xs (z `T.append` t)
    TemplateVar   t ->
      case M.lookup t vars of
        Nothing  -> concatTemplateText vars xs z
        Just val -> concatTemplateText vars xs (z `T.append` val)
    TemplateShell t -> do
      out <- runSubShell t
      concatTemplateText vars xs (z `T.append` (T.pack out))

convertValue :: Value -> CtxIO T.Text
convertValue (Text a)     = do
  return a
convertValue (Variable a) = do
  ctx <- ask
  vars <- liftIO $ readIORef (gcVars ctx)
  case M.lookup a vars of
    Nothing  -> return (T.pack "")
    Just val -> return val
convertValue (Template list) = do
  ctx <- ask
  vars <- liftIO $ readIORef (gcVars ctx)
  text <- concatTemplateText vars list (T.pack "")
  let text' = deleteManySpaces (T.unpack text)
  return (T.pack text')
  where
    deleteManySpaces :: String -> String
    deleteManySpaces [] = []
    deleteManySpaces [x] = [x]
    deleteManySpaces (' ':t@(' ':_)) = deleteManySpaces t
    deleteManySpaces (x:xs) = x : deleteManySpaces xs
convertValue (SubShell cmnds) = do
  out <- runSubShell cmnds
  return (T.pack out)

convertValues :: [Value] -> T.Text -> CtxIO T.Text
convertValues []     z = return z
convertValues (x:xs) z = do
  xVal <- convertValue x
  convertValues xs (z `T.append` xVal)

convertArgs :: [[Value]] -> [String] -> CtxIO [String]
convertArgs []     res = return res
convertArgs (x:xs) res = do
  xVal <- convertValues x (T.pack "")
  convertArgs xs (res ++ [T.unpack xVal])

printToOutput :: String -> Bool -> CtxIO ()
printToOutput str hasSpace = do
  ctx <- ask
  let strSpace = if hasSpace then str ++ [' '] else str
  if (isSubShell ctx)
    then liftIO $ modifyIORef (gcOutput ctx) (\out -> out ++ strSpace)
    else liftIO $ if hasSpace then putStrLn str else putStr str

runCommand :: Command -> CtxIO Int
runCommand (Assignment a list) = do
  bVal <- convertValues list (T.pack "")
  ctx <- ask
  liftIO $ modifyIORef (gcVars ctx) (\vars -> M.insert a bVal vars)
  return 0
runCommand (Echo hasLn list) = do
  argsString <- echoArgsToString list ""
  case argsString of
    []     -> printToOutput [] hasLn
    (_:xs) -> printToOutput xs hasLn
  return 0
  where
    echoArgsToString :: [[Value]] -> String -> CtxIO String
    echoArgsToString []     res = return res
    echoArgsToString (x:xs) res = do
      val <- convertValues x (T.pack "")
      echoArgsToString xs (res ++ (' ' : (T.unpack val)))
runCommand (Exit x)    = do
  ctx <- ask
  liftIO $ modifyIORef (gcCommands ctx) (const [])
  let retCode = (read (T.unpack x) :: Int)
  if (retCode /= 0)
    then printToOutput ("exit: " <> (T.unpack x)) True
    else return ()
  return retCode
runCommand PWD = do
  ctx <- ask
  dirName <- liftIO $ readIORef (gcPath ctx)
  printToOutput dirName True
  return 0
runCommand (CD path) = do
  pathVal <- convertValues path (T.pack "")
  goToDirs (splitPath $ T.unpack pathVal)
  where
    goToDir :: FilePath -> CtxIO Int
    goToDir p = do
      ctx <- ask
      curPath <- liftIO $ readIORef (gcPath ctx)
      let newPath = curPath </> p
      isExist <- liftIO $ doesDirectoryExist newPath
      if isExist
          then do
            if (p == ".." || p == "../")
              then liftIO $ modifyIORef (gcPath ctx) takeDirectory
              else
                if (p == "." || p == "./")
                  then return()
                  else liftIO $ modifyIORef (gcPath ctx) (const newPath)
            return 0
          else do
            liftIO $ putStrLn $ "cd: " <> p <> ": No such file or directory"
            return 2

    goToDirs :: [FilePath] -> CtxIO Int
    goToDirs []     = return 0
    goToDirs (x:xs) = do
      retCode <- goToDir x
      if (retCode == 0)
        then goToDirs xs
        else return retCode
runCommand (Read list) = do
  actionWithReadArgs list
  return 0
  where
    actionWithReadArgs :: [T.Text] -> CtxIO ()
    actionWithReadArgs []     = return ()
    actionWithReadArgs (x:xs) = do
      val <- liftIO $ getLine
      ctx <- ask
      liftIO $ modifyIORef (gcVars ctx) (\vars -> M.insert x (T.pack val) vars)
      actionWithReadArgs xs
runCommand (SubShellCommand cmnds) = do
  out <- runSubShell cmnds
  printToOutput out True
  return 0
runCommand (IfThen cond trueB falseB) = do
  case cond of
    [] -> do
      printToOutput "Empty Conditional in IfStatement" True
      return 1
    _  -> do
      case trueB of
        [] -> do
          printToOutput "Empty Body in IfStatement" True
          return 1
        _  -> do
          ctx <- ask
          liftIO $ modifyIORef (gcReturnCode ctx) (const 0)
          liftIO $ modifyIORef (gcCommands ctx)
            (\cmnds -> cond ++ ((ReadReturnCode trueB falseB) : cmnds))
          return 0
runCommand (ReadReturnCode trueB falseB) = do
  ctx <- ask
  retCode <- liftIO $ readIORef (gcReturnCode ctx)
  if (retCode == 0)
    then liftIO $ modifyIORef (gcCommands ctx) (\cmnds -> trueB ++ cmnds)
    else liftIO $ modifyIORef (gcCommands ctx) (\cmnds -> falseB ++ cmnds)
  return 0
runCommand (While cond body) = do
  case cond of
    [] -> do
      printToOutput "Empty Conditional in WhileStatement" True
      return 1
    _  -> do
      case body of
        [] -> do
          printToOutput "Empty Body in WhileStatement" True
          return 1
        _  -> do
          ctx <- ask
          liftIO $ modifyIORef (gcReturnCode ctx) (const 0)
          liftIO $ modifyIORef (gcCommands ctx)
            (\cmnds -> cond ++ ((WhileReadCode cond body) : cmnds))
          return 0
runCommand (WhileReadCode cond body) = do
  ctx <- ask
  retCode <- liftIO $ readIORef (gcReturnCode ctx)
  if (retCode == 0)
    then liftIO $ modifyIORef (gcCommands ctx)
      (\cmnds -> body ++ (cond ++ ((WhileReadCode cond body) : cmnds)))
    else return ()
  return 0
runCommand (Unknown command args) = do
  bVal <- convertValues command (T.pack "")
  argsString <- convertArgs args []
  let argsText = T.pack (concat argsString)
  let commandText = bVal `T.append` argsText
  command' <- liftIO $ parseCommand commandText
  case command' of
    (Unknown _ _) -> runExternalCommand bVal argsString
    _             -> runCommand command'

runExternalCommand :: T.Text -> [String] -> CtxIO Int
runExternalCommand command args = do
  ctx <- ask
  path <- liftIO $ readIORef (gcPath ctx)
  let command' = T.unpack command
  (_, Just hout, _, handle) <- liftIO $
    createProcess (proc command' args){ cwd = Just path,
                                        std_out = CreatePipe }
  dateOut <- liftIO $ hGetContents hout
  printToOutput dateOut False
  exitCode <- liftIO $ waitForProcess handle
  case exitCode of
    ExitSuccess   -> return 0
    ExitFailure n -> return n

runScript :: CtxIO String
runScript = do
  ctx <- ask
  cmnds <- liftIO $ readIORef (gcCommands ctx)
  case cmnds of
    []     -> do
      out <- liftIO $ readIORef (gcOutput ctx)
      return out
    (x:xs) -> do
      liftIO $ modifyIORef (gcCommands ctx) (const xs)
      retCode <- runCommand x
      ctx' <- ask
      liftIO $ modifyIORef (gcReturnCode ctx')
        (\code -> if (code /= 0) then code else retCode)
      runScript

interpret :: IO ()
interpret = getArgs >>= \case
  (script:args) -> do
    ex <- doesFileExist script
    if not ex then
      putStrLn $ "Script " <> script <> " doesn't exist"
    else do
      mapArgs <- setArgs args script
      parsedCommands <- parseScript script
      parsedCommandsRef <- newIORef parsedCommands
      returnedCodeRef <- newIORef 0
      outputRef <- newIORef ""
      dirName <- liftIO $ getCurrentDirectory
      dirNameRef <- newIORef dirName
      let ctx = Ctx mapArgs parsedCommandsRef returnedCodeRef outputRef dirNameRef False
      _ <- runReaderT runScript ctx
      return ()
  _ -> putStrLn "Expected script"
