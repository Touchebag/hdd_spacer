module Parser where

import System.Posix

data FileTree = File Int | Dir Int [FileTree]
  deriving (Show)

parseTree :: String -> IO FileTree
parseTree path = do
      file <- getFileStatus path
      if isDirectory file
        then return $ File 3
        else do
          offSize <- getFileSize file
          let size = read (show offSize) :: Int
          return $ File size

getFileSize :: FileStatus -> IO FileOffset
getFileSize stat = return $ fileSize stat
