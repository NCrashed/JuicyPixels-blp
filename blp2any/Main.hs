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
          names <- fmap (\p -> path ++ "/" ++ p) <$> getDirectoryContents path 
          blps <- filterM doesFileExist names
          mapM_ testFile blps 
        else fail "Given path is not exists!"

testFile :: FilePath -> IO ()
testFile inputFile = do 
  fc <- BS.readFile inputFile
  case parseBlp fc of 
    Left err -> fail $
      inputFile ++ ": " ++ err 
    Right blp -> putStrLn $  
      inputFile ++ ": " ++ show (blpCompression blp)
