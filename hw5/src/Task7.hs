{-# LANGUAGE RankNTypes #-}

module Task7
  ( cd
  , file
  , ls
  ) where

import Task6 (FileStructure (..), _dirs, _files)

import Lens.Micro (Traversal', (^.), filtered, traversed)

cd :: FileStructure fs => FilePath -> Traversal' fs fs
cd path = _dirs.filtered (\fs -> fs^.fsName == path)

file :: FileStructure fs => FilePath -> Traversal' fs FilePath
file path f s = if isFile s && s^.fsName == path
                 then fsName f s
                 else (_files.fsName.filtered (== path)) f s

ls :: FileStructure fs => Traversal' fs FilePath
ls = fsContents.traversed.fsName
