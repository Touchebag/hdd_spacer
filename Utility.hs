module Utility where

import Data.List
import System.Posix

import Parser

calculateTreeSize :: FileTree -> FileTree
calculateTreeSize entry@(File path size stat) = if size == -1
                  then File path (getFileSize stat) stat
                  else entry
calculateTreeSize entry@(Dir path size stat files) = if size == -1
                  then Dir path (sum $ map getDirSize tree) stat tree
                  else entry
        where tree = map calculateTreeSize files

getFileSize :: FileStatus -> Int
getFileSize stat = read (show (fileSize stat)) :: Int

getDirSize :: FileTree -> Int
getDirSize (File _ size _)       = abs size
getDirSize (Dir  _ _    _ files) = sum $ map getDirSize files

sortTree :: FileTree -> FileTree
sortTree (Dir p s f files) = Dir p s f $ sortBy (flip compare) $ map sortTree files
sortTree f = f
