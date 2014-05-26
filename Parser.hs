module Parser
  ( FileTree (File, Dir)
  , parseTree
  , showTree
  )
where

import System.Directory
import System.Posix

data FileTree = File String Int FileStatus
              | Dir  String Int FileStatus [FileTree]

instance Show FileTree where
  show = showTree (-1)

instance Eq FileTree where
  (File _ i1 _)   == (File _ i2 _)   = i1 == i2
  (File _ i1 _)   == (Dir  _ i2 _ _) = i1 == i2
  (Dir  _ i1 _ _) == (File _ i2 _)   = i1 == i2
  (Dir  _ i1 _ _) == (Dir  _ i2 _ _) = i1 == i2

instance Ord FileTree where
  (File _ i1 _)   <= (File _ i2 _)   = i1 <= i2
  (File _ i1 _)   <= (Dir  _ i2 _ _) = i1 <= i2
  (Dir  _ i1 _ _) <= (File _ i2 _)   = i1 <= i2
  (Dir  _ i1 _ _) <= (Dir  _ i2 _ _) = i1 <= i2

parseTree :: FilePath -> IO FileTree
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

showTree :: Int -> FileTree -> String
showTree = showTreeIndent ""

showTreeIndent :: String -> Int -> FileTree -> String
showTreeIndent _ 0 _ = ""
showTreeIndent indent _ (File path size _) =
          indent ++ "File: " ++ path
          ++ " -- Size: " ++ show size ++ "\n"
showTreeIndent indent depth (Dir  path size _ files) =
          indent ++ "Dir: "  ++ path
          ++ " -- Size: " ++ show size ++ "\n"
          ++ concatMap (showTreeIndent ("  " ++ indent) (max (depth - 1) (-1))) files

--testTree :: IO FileTree
--testTree = do t <- parseTree "test"
--              return $ calculateTreeSize t
