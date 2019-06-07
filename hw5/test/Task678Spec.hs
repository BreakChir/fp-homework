module Task678Spec
  ( spec
  ) where

import Lens.Micro ((^..), (^?), (.~), (&))

import Task6 (getPath, move, scanDir)
import Task7 (cd, file, ls)
import Task8 (_allEntries, _filesExt, changeExt, removeDir)

import Test.Hspec (Spec, describe, it)

someFunc :: IO ()
someFunc = do
  dir <- scanDir "test/testDirectory"
  case dir of
    Nothing    -> putStrLn "Given path is not directory or file"
    Just myDir -> do
      putStrLn ">>  myDir ^.. cd \"dir1\" . ls"
      putStrLn . show $ myDir ^.. cd "dir1" . ls

      putStrLn ">>  myDir ^? cd \"dir1\" . cd \"dir1_1\" . file \"kek1.pepe.hs\""
      putStrLn . show $ myDir ^? cd "dir1" . cd "dir1_1" . file "kek1.pepe.hs"

      putStrLn ">>  myDir ^.. _allEntries"
      putStrLn . show $ myDir ^.. _allEntries

      putStrLn ">>  myDir & cd \"dir1\" . _filesExt .~ \".cpp\""
      putStrLn . show $ myDir & cd "dir1" . _filesExt .~ ".cpp"

      putStrLn
        ">>  (myDir & _filesExt .~ \".hs\") & cd \"dir1\" . _filesExt .~ \".cpp\""
      putStrLn . show $
        (myDir & _filesExt .~ ".hs") & cd "dir1" . _filesExt .~ ".cpp"

      putStrLn ">>  myDir ^.. removeDir \"emptyDir\""
      putStrLn . show $ myDir ^.. removeDir "emptyDir"

      putStrLn ">>  myDir ^? removeDir \"emptyDir\" . cd \"emptyDir\""
      putStrLn . show $ myDir ^? removeDir "emptyDir" . cd "emptyDir"

      putStrLn ">>  myDir ^.. changeExt \".cpp\" .removeDir \"emptyDir\" . _allEntries"
      putStrLn . show $ myDir ^.. changeExt ".cpp" .removeDir "emptyDir" . _allEntries

      putStrLn ">>  myDir ^? move \"dir1\" . move \"dir1_1\" . getPath"
      putStrLn . show $ myDir ^? move "dir1" . move "dir1_1" . getPath

      putStrLn ">>  myDir ^? move \"dir1\" . move \"dir1_1\" . move \"kek1.pepe.hs\" . getPath"
      putStrLn . show $ myDir ^? move "dir1" . move "dir1_1" . move "kek1.pepe.hs" . getPath

spec :: Spec
spec =
  describe "Lens FS TESTS" $ do
    it "checks some lens and traversal" $
      someFunc
