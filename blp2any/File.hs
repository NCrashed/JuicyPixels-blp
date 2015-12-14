module File(
    forEachFile
  , forEachFile'
  ) where

import System.Directory
import System.FilePath
import Control.Monad 

-- | Performs width traversion of given directory
forEachFile :: FilePath -- ^ Directory to traverse
  -> (FilePath -> Bool) -- ^ File selector
  -> (a -> FilePath -> IO a) -- ^ Function to perform (mimics fold)
  -> a -- ^ Start value of acum
  -> IO a -- ^ Last value of accum
forEachFile path predf f acc = go acc path
  where
    go a inputPath = do 
      names <- fmap (\p -> inputPath ++ "/" ++ p) <$> getDirectoryContents inputPath
      dirs <- filter (not . isSystemDir) <$> filterM doesDirectoryExist names
      files <- filter predf <$> filterM doesFileExist names
      a' <- foldM f a files 
      a' `seq` foldM go a' dirs

    isSystemDir s = let bs = takeFileName s in bs == "." || bs == ".."

-- | Performs width traversion of given directory, simplified versino of forEachFile
forEachFile' :: FilePath -- ^ Directory to traverse
  -> (FilePath -> Bool) -- ^ File selector
  -> (FilePath -> IO ()) -- ^ Function to perform
  -> IO ()
forEachFile' path predf f = forEachFile path predf (const f) ()