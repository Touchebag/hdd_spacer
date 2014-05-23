module Parser where

import System.Directory
import System.Posix

data FileTree = File String FileStatus | Dir String FileStatus [FileTree]

instance Show FileTree where
  show (File path _)   = "File: " ++ path
  show (Dir  path _ files) = "Dir: "  ++ path ++ "\n" ++ unlines (map show files)

parseTree :: String -> IO FileTree
parseTree path = do
      file <- getFileStatus path
      if isDirectory file
        then do
          files <- getDirectoryContents path
          tree  <- mapM parseTree $ filterDirContents path files
          return $ Dir path file tree
        else return $ File path file

filterDirContents :: FilePath -> [FilePath] -> [FilePath]
filterDirContents _    []        = []
filterDirContents path (".":fs)  = filterDirContents path fs
filterDirContents path ("..":fs) = filterDirContents path fs
filterDirContents path (f:fs)    = (path ++ "/" ++ f) : filterDirContents path fs

getFileSize :: FileStatus -> Int
getFileSize stat = read (show (fileSize stat)) :: Int

getDirSize :: FileStatus -> Int
getDirSize = undefined
