module File(
    forEachFile
  , tryAll
  ) where

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath

-- | Fold over the files of a directory that pass a predicate, breadth first.
forEachFile :: Bool -- ^ Whether to descend into subdirectories
  -> FilePath -- ^ Directory to traverse
  -> (FilePath -> Bool) -- ^ File selector
  -> (a -> FilePath -> IO a) -- ^ Step, called once per file
  -> a -- ^ Initial accumulator
  -> IO a
forEachFile recurse path predf f acc = go acc path
  where
    go a dir = do
      names <- fmap (dir </>) <$> listDirectory dir
      files <- filter predf <$> filterM doesFileExist names
      a' <- foldM f a files
      if recurse
        then filterM doesDirectoryExist names >>= foldM go a'
        else return a'

-- | Run an action, turning any synchronous exception into 'Left'.
-- Asynchronous exceptions (such as Ctrl+C) are rethrown.
tryAll :: IO a -> IO (Either String a)
tryAll action = do
  result <- try action
  case result of
    Right a -> return (Right a)
    Left e
      | Just async <- fromException e -> throwIO (async :: SomeAsyncException)
      | otherwise -> return . Left $ displayException (e :: SomeException)
