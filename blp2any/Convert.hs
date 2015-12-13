module Convert(
    convertFile
  ) where

import Codec.Picture
import Codec.Picture.Blp
import Control.Monad 
import System.Directory 
import System.FilePath

import qualified Data.ByteString as BS

convertFile :: FilePath -> FilePath -> IO ()
convertFile inputPath outputPath = do 
  efx <- doesFileExist inputPath 
  if efx then testFile inputPath False outputPath
    else do 
      edx <- doesDirectoryExist inputPath 
      if edx then do 
        names <- fmap (\p -> inputPath ++ "/" ++ p) <$> getDirectoryContents inputPath 
        blps <- filterM doesFileExist names
        mapM_ (\blp -> testFile blp True outputPath) blps 
      else fail "Given path is not exists!"

testFile :: FilePath -> Bool -> FilePath -> IO ()
testFile inputFile isDir outputPath = do 
  fc <- BS.readFile inputFile
  img <- case decodeBlp fc of 
    Left err -> fail $ inputFile ++ ": " ++ err 
    Right img -> return img 

  when isDir $ createDirectoryIfMissing True outputPath
  let outputFile = if isDir 
      then outputPath ++ "/" ++ takeFileName inputFile
      else outputPath

  res <- writeDynamicPng outputFile img
  case res of 
    Left err -> fail $ inputFile ++ ": " ++ err 
    Right flag -> putStrLn $ inputFile ++ ": " ++ show flag