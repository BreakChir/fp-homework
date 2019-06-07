{-# LANGUAGE RankNTypes #-}

module Task8
  ( _allEntries
  , _extension
  , _filesExt
  , changeExt
  , removeDir
  ) where

import Task6 (FileStructure (..), _files)

import Data.List (elemIndices)
import Lens.Micro (SimpleGetter, Traversal', (^.), (.~), (%~), to, traversed)

_extension :: Traversal' FilePath String
_extension f file = (name ++) <$> f ext
  where
    dotIndex = case elemIndices '.' file of
                 [] -> length file
                 x  -> last x
    (name, ext) = splitAt dotIndex file

_filesExt :: FileStructure fs => Traversal' fs String
_filesExt = _files.fsName._extension

_allEntries :: FileStructure fs => Traversal' fs FilePath
_allEntries = fsContents . traversed . (\f s -> fsName f s *> _allEntries f s)

changeExt :: FileStructure fs => String -> SimpleGetter fs fs
changeExt ext = to $ _files.fsName._extension .~ ext

removeDir :: FileStructure fs => FilePath -> SimpleGetter fs fs
removeDir fp = to $ fsContents %~ (filter rmPredicate)
  where
    rmPredicate f = not ((not . isFile) f && f^.fsName == fp && null (f^.fsContents))
