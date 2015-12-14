module Main where

import Options.Applicative.Simple

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

-- | How to parse BlpFormat
blpFormatR :: ReadM BlpFormat 
blpFormatR = readMPure "blp format" readBlpFormat

main :: IO ()
main = do 
  (_,runCmd) <- simpleOptions ver headerDesc desc (pure ()) $ do 
    addCommand "convert" "Converts given blp or folder with blps to PNG" convertFiles $ (<*>) helper $ ConvertOptions
      <$> strArgument (metavar "INPUT_PATH" <> help "input file or directory (batch mode)")
      <*> strArgument (metavar "OUTPUT_PATH" <> help "output file name or directory (need explicit format option)")
      <*> option convertFormatR (long "format" <> short 'f' <> value UnspecifiedFormat <> help "Output file format, if not specified tries to infer from output file name. Values: blp png jpg tiff gif bmp")
      <*> option auto (long "quality" <> short 'q' <> value 90 <> help "Quality level for formats with lossy compression (default 90)")
      <*> fmap not (switch (long "notPreserveStructure" <> short 'p' <> help "Will not produce subfolders while batch converting" ))
      <*> switch (long "shallow" <> short 's' <> help "Not look into subfolders while batch converting")
      <*> option blpFormatR (long "blpFormat" <> short 'b' <> value BlpJpeg <> help "Specifies what BLP internal format to use when converting to BLP. Values: jpg uncompressedWithAlpha uncompressedWithoutAlpha")
    addCommand "stats" "Collects statistics about BLP images in folder and saves examples of BLP's for each sample" (uncurry collectStatistics) $ (<*>) helper $ (,)
      <$> strArgument (metavar "DIRECTORY" <> help "Input folder that stores blps (subfolders are processed)")
      <*> strArgument (metavar "OUTPUT_PATH" <> help "Here blp samples would be stored")
  runCmd
  where 
    ver = "0.1.0.0"
    headerDesc = ""
    desc = "Converting BLP1 Warcraft III format to/from PNG, JPEG, TGA, TIFF, BMP, GIF"