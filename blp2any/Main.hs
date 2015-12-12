module Main where

import Codec.Picture
import Codec.Picture.Blp
import System.Environment
import System.Directory 
import Control.Monad 

import qualified Data.ByteString as BS

main :: IO ()
main = do 
  inputPaths <- getArgs
  forM_ inputPaths $ \path -> do 
    efx <- doesFileExist path 
    if efx then testFile path 
      else do 
        edx <- doesDirectoryExist path 
        if edx then do 
          names <- getDirectoryContents path 
          mapM_ testFile names 
        else fail "Given path is not exists!"

testFile :: FilePath -> IO ()
testFile inputFile = do 
  fc <- BS.readFile inputFile
  case parseBlp fc of 
    Left err -> fail err 
    Right blp -> print blp
