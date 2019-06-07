{-# LANGUAGE RankNTypes #-}

module Task6
  ( FileStructure (..)
  , _dirs
  , _files
  , scanDir
  ) where

import Control.Monad (mapM)
import Data.Maybe (catMaybes)
import Lens.Micro (Traversal', (^.), (.~), (&), filtered, traversed)
import System.Directory
  (doesDirectoryExist, doesFileExist, listDirectory, withCurrentDirectory)

import Task5 (Lens', lens)

data FS
  = Dir
  { name     :: FilePath
  , contents :: [FS]
  }
  | File
  { name     :: FilePath
  } deriving Show

-- Hard task(Task 8) with @move@ and @getPath@
data ExpendedFS = ExpendedFS
  { structure :: FS
  , fsPath    :: FilePath
  } deriving Show

class FileStructure fs where
  fsName     :: Lens' fs FilePath
  fsContents :: Lens' fs [fs]
  isFile     :: fs -> Bool
  getPath    :: Lens' fs FilePath
  move       :: FilePath -> Traversal' fs ExpendedFS

instance FileStructure FS where
  fsName        = _name
  fsContents    = _contents
  isFile        = isFileFS
  getPath       = _name
  move path     = cast' . _move path
    where
      cast' :: Lens' FS ExpendedFS
      cast' = lens (\s -> ExpendedFS s "root/") (\_ v -> structure v)

instance FileStructure ExpendedFS where
  fsName     = _fs._name
  fsContents = lens getter setter
    where
      setter :: ExpendedFS -> [ExpendedFS] -> ExpendedFS
      setter s efs = s & _fs._contents .~ (map structure efs)

      getter :: ExpendedFS -> [ExpendedFS]
      getter s = let cnts = s^._fs._contents
                     pathSuffix fs = if (isFile fs) then "" else "/"
                     args = map (\fs -> (fs, fsPath s <> fs^._name <> pathSuffix fs)) cnts
                  in uncurry ExpendedFS <$> args

  isFile     = isFileFS . structure
  getPath    = lens fsPath (\fs v -> fs { fsPath = v })
  move path  = _move path

scanDir :: FilePath -> IO (Maybe FS)
scanDir path = do
  isFileExist <- doesFileExist path
  if isFileExist
    then return $ Just (File path)
    else do
      isDirExist <- doesDirectoryExist path
      if isDirExist
        then withCurrentDirectory path $ do
          entries <- listDirectory "."
          entriesFS <- mapM scanDir entries
          let contents' = catMaybes entriesFS
          return $ Just (Dir path contents')
        else return Nothing

_name :: Lens' FS FilePath
_name = lens name (\fs v -> fs { name = v })

_contents :: Lens' FS [FS]
_contents f fs@(Dir _ cnts) = (\v -> fs { contents = v }) <$> f cnts
_contents f fs@(File _)     = const fs <$> f []

isFileFS :: FS -> Bool
isFileFS (File _) = True
isFileFS _        = False

_fs :: Lens' ExpendedFS FS
_fs = lens structure (\fs v -> fs { structure = v })

_files :: FileStructure fs => Traversal' fs fs
_files = fsContents.traversed.filtered isFile

_dirs :: FileStructure fs => Traversal' fs fs
_dirs = fsContents.traversed.filtered (not . isFile)

_move :: FileStructure fs => FilePath -> Traversal' fs fs
_move path = fsContents.traversed.filtered (\fs -> fs^.fsName == path)
