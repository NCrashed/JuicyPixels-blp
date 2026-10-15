module Main where

import Data.Version (showVersion)
import Options.Applicative.Simple

import qualified Paths_JuicyPixels_blp as Paths

import Statistics
import Convert

-- | Helper for lifting pure parsing functions into ReadM monad
readMPure :: String -- ^ Name of argument for pretty error message
  -> (String -> Maybe a) -- ^ Function that parses the argument
  -> ReadM a -- ^ Optparse parser monad
readMPure desc f = do
  s <- str
  case f s of
    Nothing -> fail $ "Unknown " ++ desc ++ " " ++ s
    Just v -> return v

-- | How to parse ConvertFormat
convertFormatR :: ReadM ConvertFormat
convertFormatR = readMPure "format" readConvertFormat

-- | JPEG quality, 1 to 100.
qualityR :: ReadM Int
qualityR = do
  q <- auto
  if q >= 1 && q <= 100 then return q else fail "quality must be 1 to 100"

-- | How to parse BlpKind
blpKindR :: ReadM BlpKind
blpKindR = readMPure "blp format" readBlpKind

main :: IO ()
main = do
  (_,runCmd) <- simpleOptions ver headerDesc desc (pure ()) $ do
    addCommand "convert" "Converts a file or a folder between BLP and PNG, JPEG, TIFF, GIF or BMP" convertFiles $ ConvertOptions
      <$> strArgument (metavar "INPUT_PATH" <> help "input file or directory (batch mode)")
      <*> strArgument (metavar "OUTPUT_PATH" <> help "output file name or directory (need explicit format option)")
      <*> option convertFormatR (long "input-format" <> short 'i' <> value UnspecifiedFormat <> help "Input file format, if not specified tries to infer from input file name. Values: blp png jpg tiff gif bmp.")
      <*> option convertFormatR (long "format" <> short 'f' <> value UnspecifiedFormat <> help "Output file format, if not specified tries to infer from output file name. Values: blp png jpg tiff gif bmp.")
      <*> option qualityR (long "quality" <> short 'q' <> value 90 <> help "Quality level for formats with lossy compression, 1 to 100 (default 90)")
      <*> fmap not (switch (long "flatten" <> long "notPreserveStructure" <> short 'p' <> help "Write every converted file straight into OUTPUT_PATH instead of recreating subfolders (files with the same name overwrite each other)"))
      <*> switch (long "shallow" <> short 's' <> help "Only convert files directly in INPUT_PATH, not in its subfolders")
      <*> option blpKindR (long "blpFormat" <> short 'b' <> value BlpJpeg <> help "Specifies what BLP internal format to use when converting to BLP. Values: jpg uncompressedWithAlpha uncompressedWithoutAlpha")
      <*> option auto (long "min-mipmap-size" <> value 1 <> help "Smallest mipmap side to keep when writing BLP (default 1)")
      <*> optional (option auto (long "timeout" <> metavar "SECONDS" <> help "Give up on a file after this many seconds; some corrupt JPEG data makes decoding hang"))
      <*> fmap not (switch (long "no-dither" <> help "Do not dither paletted BLP textures; better for icons and pixel art"))
      <*> optional (option auto (long "max-size" <> metavar "PIXELS" <> help "When writing BLP, halve the image until its larger side is at most this; Warcraft III shows mip levels up to 512 only"))
    addCommand "stats" "Groups the BLPs in a folder by header values and saves one example of each group" (uncurry collectStatistics) $ (,)
      <$> strArgument (metavar "DIRECTORY" <> help "Input folder that stores blps (subfolders are processed)")
      <*> strArgument (metavar "OUTPUT_PATH" <> help "Here blp samples would be stored")
  runCmd
  where
    ver = showVersion Paths.version
    headerDesc = ""
    desc = "Converting BLP1 Warcraft III textures to/from PNG, JPEG, TIFF, GIF and BMP"
