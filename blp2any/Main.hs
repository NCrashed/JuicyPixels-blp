module Main where

import Codec.Picture
import Codec.Picture.Blp
import Codec.Picture.Types
import Control.Monad 
import System.Directory 
import System.Environment
import System.FilePath

import qualified Data.ByteString as BS

main :: IO ()
main = do 
  [inputPath, outputPath] <- getArgs
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

  {-
  putStrLn $ case img of 
    ImageY8 _ -> "ImageY8"
    ImageY16 _ -> "ImageY16"
    ImageYF _ -> "ImageYF"
    ImageYA8 _ -> "ImageYA8"
    ImageYA16 _ -> "ImageYA16"
    ImageRGB8 _ -> "ImageRGB8"
    ImageRGB16 _ -> "ImageRGB16"
    ImageRGBF _ -> "ImageRGBF"
    ImageRGBA8 _ -> "ImageRGBA8"
    ImageRGBA16 _ ->  "ImageRGBA16"
    ImageYCbCr8 _ ->  "ImageYCbCr8"
    ImageCMYK8 _ ->  "ImageCMYK8"
    ImageCMYK16 _ ->  "ImageCMYK16"
  -}

  res <- writeDynamicPng outputFile $ toPngRepresentable img
  case res of 
    Left err -> fail $ inputFile ++ ": " ++ err 
    Right flag -> putStrLn $ inputFile ++ ": " ++ show flag
  
toPngRepresentable :: DynamicImage -> DynamicImage 
toPngRepresentable i = case i of 
  ImageY8 _ -> i 
  ImageY16 _ -> i
  ImageYF p -> ImageRGB16 . convertFloatImage . promoteImage $ p
  ImageYA8 _ -> i
  ImageYA16 _ -> i
  ImageRGB8 _ -> i 
  ImageRGB16 _ -> i
  ImageRGBF p -> ImageRGB16 . convertFloatImage $ p
  ImageRGBA8 _ -> i 
  ImageRGBA16 _ -> i
  ImageYCbCr8 p -> ImageRGB8 . convertImage $ p
  ImageCMYK8 p -> ImageRGBA8 . convertCMYK8Image $ p 
  ImageCMYK16 p -> ImageRGBA16 . convertCMYK16Image $ p

convertFloatImage :: Image PixelRGBF -> Image PixelRGB16
convertFloatImage = pixelMap convert 
  where 
    convert (PixelRGBF rf gf bf) = PixelRGB16 
      (round $ 65535 * rf)
      (round $ 65535 * gf)
      (round $ 65535 * bf)

convertCMYK8Image :: Image PixelCMYK8 -> Image PixelRGBA8 
convertCMYK8Image = pixelMap convert 
  where 
  convert (PixelCMYK8 c m y k) =
      PixelRGBA8 (clampWord8 r) (clampWord8 g) (clampWord8 b) k
    where
    clampWord8 = fromIntegral . max 0 . min 255 . (`div` 255)

    ik :: Int
    ik = fromIntegral k
    r = fromIntegral y * ik 
    g = fromIntegral m * ik 
    b = fromIntegral c * ik 

convertCMYK16Image :: Image PixelCMYK16 -> Image PixelRGBA16 
convertCMYK16Image = pixelMap convert 
  where 
  convert (PixelCMYK16 c m y k) =
      PixelRGBA16 (clampWord16 r) (clampWord16 g) (clampWord16 b) 65535
    where
    clampWord16 = fromIntegral . max 0 . min 65535 . (`div` 65535)

    ik :: Int
    ik = fromIntegral k
    r = fromIntegral y * ik 
    g = fromIntegral m * ik 
    b = fromIntegral c * ik 