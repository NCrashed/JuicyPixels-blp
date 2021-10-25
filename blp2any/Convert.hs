module Convert(
    convertFiles
  , ConvertFormat(..)
  , ConvertOptions(..)
  , readConvertFormat
  , BlpFormat(..)
  , readBlpFormat
  ) where

import Codec.Picture
import Codec.Picture.Blp
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Monoid
import System.Directory
import System.FilePath

import qualified Data.ByteString as BS

import File

data BlpFormat =
    BlpJpeg
  | BlpUncompressedWithAlpha
  | BlpUncompressedWithoutAlpha
  deriving (Eq, Ord, Show, Enum, Bounded)

readBlpFormat :: String -> Maybe BlpFormat
readBlpFormat s = case toLower <$> s of
  "jpeg" -> Just BlpJpeg
  "jpg" -> Just BlpJpeg
  "uncompressed1" -> Just BlpUncompressedWithAlpha
  "uncompressedwithalpha" -> Just BlpUncompressedWithAlpha
  "uncompressed2" -> Just BlpUncompressedWithoutAlpha
  "uncompressedwithoutalpha" -> Just BlpUncompressedWithoutAlpha
  _ -> Nothing

data ConvertFormat =
    Blp
  | Png
  | Jpeg
  | Tiff
  | Gif
  | Bmp
  | UnspecifiedFormat
  deriving (Eq, Ord, Show, Enum, Bounded)

readConvertFormat :: String -> Maybe ConvertFormat
readConvertFormat s = case toLower <$> s of
  "blp" -> Just Blp
  "png" -> Just Png
  "jpeg" -> Just Jpeg
  "jpg" -> Just Jpeg
  "jpe" -> Just Jpeg
  "jif" -> Just Jpeg
  "jfif" -> Just Jpeg
  "jfi" -> Just Jpeg
  "tiff" -> Just Tiff
  "gif" -> Just Gif
  "bmp" -> Just Bmp
  _ -> Nothing

-- | Replaces Unspecified
defaultFormat :: ConvertFormat
defaultFormat = Png

formatExtension :: ConvertFormat -> [String]
formatExtension c = case c of
  Blp -> [".blp"]
  Png -> [".png"]
  Jpeg -> [".jpeg", ".jpg", ".jpe", ".jif", ".jfif", ".jfi"]
  Tiff -> [".tiff"]
  Gif -> [".gif"]
  Bmp -> [".bmp"]
  UnspecifiedFormat -> []

supportedExtensions :: [String]
supportedExtensions = concat $ formatExtension <$> [Blp .. Bmp]

data ConvertOptions = ConvertOptions {
  convertInput          :: FilePath
, convertOutput         :: FilePath
, convertInputFormat    :: ConvertFormat
, convertFormat         :: ConvertFormat
, convertQuality        :: Int
, convertPreservesDirs  :: Bool
, convertShallow        :: Bool
, convertBlpFormat      :: BlpFormat
, convertBlpMinMipSize  :: Int
}

convertFiles :: ConvertOptions -> IO ()
convertFiles opts@ConvertOptions{..} = do
  efx <- doesFileExist convertInput
  if efx
    then convertFile opts False convertInput
    else do
      edx <- doesDirectoryExist convertInput
      if edx then do 
        putStrLn $ "Converting all files from " ++ convertInput ++ " folder"
        forEachFile' convertInput fileFilter $ convertFile opts True
      else fail $ "Given path " ++ convertInput ++ " doesn't exsist!"
  where
    fileFilter s = case convertFormat of
      Blp -> (fmap toLower $ takeExtension s) `elem` supportedExtensions
      _ -> (".blp" ==) . fmap toLower . takeExtension $ s

-- | Trye to guess format from name of file
guessFormat :: FilePath -> Maybe ConvertFormat
guessFormat = readConvertFormat . drop 1 . takeExtension

-- | Try to load input file with desired format
readInputFile :: FilePath -> ConvertFormat -> IO DynamicImage
readInputFile inputFile format = case format of
  Blp -> do
    fc <- BS.readFile inputFile
    case decodeBlp fc of
      Left err -> fail $ "Failed to load file " <> inputFile <> ", parse error: " <> err
      Right img -> pure img
  Png -> loadJuicy readPng
  Jpeg -> loadJuicy readJpeg
  Tiff -> loadJuicy readTiff
  Gif -> loadJuicy readGif
  Bmp -> loadJuicy readBitmap
  UnspecifiedFormat -> case guessFormat inputFile of
    Just newFormat -> readInputFile inputFile newFormat
    Nothing -> fail $ "Cannot infer format from filename " <> inputFile
  where
    loadJuicy :: (FilePath -> IO (Either String DynamicImage)) -> IO DynamicImage
    loadJuicy f = do
      mres <- f inputFile
      case mres of
        Left err -> fail $ "Failed to load file " <> inputFile <> " as " <> show format <> ", error: " <> err
        Right img -> pure img

convertFile :: ConvertOptions -> Bool -> FilePath -> IO ()
convertFile ConvertOptions{..} isDir inputFile = do
  img <- readInputFile inputFile convertInputFormat
  let distFormat = case convertFormat of
        UnspecifiedFormat -> if isDir then defaultFormat
          else case guessFormat convertOutput of
            Just f -> f
            Nothing -> defaultFormat
        _ -> convertFormat

  when isDir $ createDirectoryIfMissing True convertOutput
  let outputFile = if isDir
      then convertOutput 
        </> drop (length convertInput + 1) (takeDirectory inputFile)
        </> (takeBaseName inputFile <> fromMaybe "" (listToMaybe $ formatExtension distFormat))
      else convertOutput
  createDirectoryIfMissing True $ takeDirectory outputFile

  let mipsCount = mipMapsUpTo convertBlpMinMipSize img
  res <- convertionFunction distFormat convertQuality convertBlpFormat outputFile mipsCount img
  case res of
    Left err -> fail $ inputFile <> ": " <> err
    Right _ -> putStrLn $ inputFile <> ": Success"

convertionFunction :: ConvertFormat -> Int -> BlpFormat -> FilePath -> Int -> DynamicImage -> IO (Either String ())
convertionFunction f quality blpFormat path mipsCount img = case f of
  Blp -> case blpFormat of
    BlpJpeg -> writeBlpJpeg path quality mipsCount img >> pure (Right ())
    BlpUncompressedWithAlpha -> writeBlpUncompressedWithAlpha path mipsCount img >> pure (Right ())
    BlpUncompressedWithoutAlpha -> writeBlpUncompressedWithoutAlpha path mipsCount img >> pure (Right ())
  Png -> do
    res <- writeDynamicPng path img
    pure $ void res
  Jpeg -> saveJpgImage quality path img >> pure (Right ())
  Tiff -> saveTiffImage path img >> pure (Right ())
  Gif -> case saveGifImage path img of
    Left er -> pure $ Left er
    Right io -> io >> pure (Right ())
  Bmp -> saveBmpImage path img >> pure (Right ())
  UnspecifiedFormat -> pure $ Left "no conversion format specified"
