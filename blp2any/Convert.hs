{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
module Convert(
    convertFiles
  , ConvertFormat(..)
  , ConvertOptions(..)
  , readConvertFormat
  , BlpKind(..)
  , readBlpKind
  ) where

import Codec.Picture
import Codec.Picture.Blp
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Char
import Data.Maybe
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.Timeout (timeout)

import qualified Data.ByteString as BS

import File

-- | BLP content type to write, as picked on the command line.
data BlpKind =
    BlpJpeg
  | BlpUncompressedWithAlpha
  | BlpUncompressedWithoutAlpha
  deriving (Eq, Ord, Show, Enum, Bounded)

readBlpKind :: String -> Maybe BlpKind
readBlpKind s = case toLower <$> s of
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
  "tif" -> Just Tiff
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
  Tiff -> [".tiff", ".tif"]
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
, convertBlpKind        :: BlpKind
, convertBlpMinMipSize  :: Int
, convertTimeout        :: Maybe Int
, convertDither         :: Bool
, convertMaxSize        :: Maybe Int
}

convertFiles :: ConvertOptions -> IO ()
convertFiles opts@ConvertOptions{..} = do
  isFile <- doesFileExist convertInput
  isDir <- doesDirectoryExist convertInput
  if | isFile -> withTimeout convertTimeout (convertFile opts False convertInput) >>= either (failWith . ((convertInput ++ ": ") ++)) putStrLn
     | isDir -> do
         putStrLn $ "Converting all files from " ++ convertInput ++ " folder"
         (converted, failed) <- forEachFile (not convertShallow) convertInput fileFilter step (0 :: Int, 0 :: Int)
         putStrLn $ show converted ++ " converted, " ++ show failed ++ " failed"
         when (failed > 0) exitFailure
     | otherwise -> failWith $ "Given path " ++ convertInput ++ " doesn't exist!"
  where
    step (!converted, !failed) inputFile = do
      result <- tryAll (withTimeout convertTimeout (convertFile opts True inputFile))
      case join result of
        Right msg -> putStrLn msg >> return (converted + 1, failed)
        Left err -> hPutStrLn stderr (inputFile ++ ": " ++ err) >> return (converted, failed + 1)
    fileFilter s = case convertFormat of
      Blp -> (fmap toLower $ takeExtension s) `elem` supportedExtensions
      _ -> (".blp" ==) . fmap toLower . takeExtension $ s
    failWith err = hPutStrLn stderr err >> exitFailure

-- | Trye to guess format from name of file
guessFormat :: FilePath -> Maybe ConvertFormat
guessFormat = readConvertFormat . drop 1 . takeExtension

-- | Try to load input file with desired format
readInputFile :: FilePath -> ConvertFormat -> IO (Either String DynamicImage)
readInputFile inputFile format = case format of
  Blp -> either (Left . ("parse error: " <>)) Right . decodeBlp <$> BS.readFile inputFile
  Png -> loadJuicy readPng
  Jpeg -> loadJuicy readJpeg
  Tiff -> loadJuicy readTiff
  Gif -> loadJuicy readGif
  Bmp -> loadJuicy readBitmap
  UnspecifiedFormat -> case guessFormat inputFile of
    Just newFormat -> readInputFile inputFile newFormat
    Nothing -> pure . Left $ "cannot infer format from the file name"
  where
    loadJuicy :: (FilePath -> IO (Either String DynamicImage)) -> IO (Either String DynamicImage)
    loadJuicy f = either (Left . (("cannot load as " <> show format <> ": ") <>)) Right <$> f inputFile

-- | Convert one file; the result is a line to report on success.
convertFile :: ConvertOptions -> Bool -> FilePath -> IO (Either String String)
convertFile ConvertOptions{..} isDir inputFile = runExceptT $ do
  img <- ExceptT $ readInputFile inputFile convertInputFormat
  distFormat <- case convertFormat of
    UnspecifiedFormat
      | isDir -> pure defaultFormat
      | otherwise -> maybe
          (throwE $ "cannot tell the output format from " <> convertOutput <> "; give it with -f")
          pure (guessFormat convertOutput)
    _ -> pure convertFormat
  let outputName = takeBaseName inputFile <> fromMaybe "" (listToMaybe $ formatExtension distFormat)
      -- Keep the input's subdirectory unless asked to flatten the output.
      outputFile
        | not isDir = convertOutput
        | convertPreservesDirs = convertOutput </> makeRelative convertInput (takeDirectory inputFile) </> outputName
        | otherwise = convertOutput </> outputName
  sameFile <- liftIO $ equalFilePath <$> makeAbsolute inputFile <*> makeAbsolute outputFile
  when sameFile . throwE $ "the output " <> outputFile <> " would overwrite the input"
  liftIO . createDirectoryIfMissing True $ takeDirectory outputFile

  let mipsCount = mipMapsUpTo convertBlpMinMipSize img
  let encodeOptions = defaultBlpEncodeOptions { blpMipLevels = mipsCount, blpDither = convertDither, blpMaxSize = convertMaxSize }
  ExceptT $ convertionFunction distFormat convertQuality convertBlpKind encodeOptions outputFile img
  pure $ inputFile <> ": Success"

-- | A readable message for an encoding error.
describeEncodeError :: BlpEncodeError -> String
describeEncodeError e = case e of
  BlpEmptyImage -> "the image is empty"
  BlpImageTooLarge w h -> "the image is " <> show w <> "x" <> show h <> ", above the 65535 pixel limit of BLP"
  BlpInvalidJpegQuality q -> "JPEG quality must be 1 to 100, got " <> show q
  BlpFileTooLarge -> "the BLP file would be larger than 4 GiB"
  BlpInvalidMaxSize n -> "--max-size must be at least 1, got " <> show n

-- | Run a conversion, giving up after the given number of seconds.
withTimeout :: Maybe Int -> IO (Either String a) -> IO (Either String a)
withTimeout Nothing action = action
withTimeout (Just seconds) action =
  fromMaybe (Left $ "gave up after " <> show seconds <> " s") <$> timeout (seconds * 1000000) action

convertionFunction :: ConvertFormat -> Int -> BlpKind -> BlpEncodeOptions -> FilePath -> DynamicImage -> IO (Either String ())
convertionFunction f quality blpKind encodeOptions path img = case f of
  Blp -> do
    let format = case blpKind of
          BlpJpeg -> BlpFormatJpeg quality
          BlpUncompressedWithAlpha -> BlpFormatPaletted True
          BlpUncompressedWithoutAlpha -> BlpFormatPaletted False
    res <- writeBlpWith path encodeOptions { blpFormat = format } img
    pure $ either (Left . describeEncodeError) Right res
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
