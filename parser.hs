module Parser where

import System.Directory
import System.Posix

data FileTree = File String Int FileStatus | Dir String Int FileStatus [FileTree]

instance Show FileTree where
  show (File path size _)   = "File: " ++ path ++ "\n Size: " ++ show size
  show (Dir  path size _ files) = "Dir: "  ++ path ++
                "\n Size: " ++ show size ++ "\n" ++ unlines (map show files)

parseTree :: String -> IO FileTree
parseTree path = do
      file <- getFileStatus path
      if isDirectory file
        then do
          files <- getDirectoryContents path
          tree  <- mapM parseTree $ filterDirContents path files
          return $ Dir path (-1) file tree
        else return $ File path (-1) file

filterDirContents :: FilePath -> [FilePath] -> [FilePath]
filterDirContents _    []        = []
filterDirContents path (".":fs)  = filterDirContents path fs
filterDirContents path ("..":fs) = filterDirContents path fs
filterDirContents path (f:fs)    = (path ++ "/" ++ f) :
                                    filterDirContents path fs

getFileSize :: FileStatus -> Int
getFileSize stat = read (show (fileSize stat)) :: Int

calculateTreeSize :: FileTree -> FileTree
calculateTreeSize entry@(File path size stat) = if size == -1
                  then File path (getFileSize stat) stat
                  else entry
calculateTreeSize entry@(Dir path size stat files) = if size == -1
                  then Dir path (sum $ map getDirSize tree) stat tree
                  else entry
        where tree = map calculateTreeSize files

getDirSize :: FileTree -> Int
getDirSize (File _ size _)       = abs size
getDirSize (Dir  _ _    _ files) = sum $ map getDirSize files
