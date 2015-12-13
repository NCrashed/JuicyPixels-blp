module Main where

import Options.Applicative.Simple

import Statistics
import Convert

main :: IO ()
main = do 
  (_,runCmd) <- simpleOptions ver headerDesc desc (pure ()) $ do 
    addCommand "convert" "Converts given blp or folder with blps to PNG" (uncurry convertFile) $ (,)
      <$> strOption (long "input" <> short 'i' <> metavar "INPUT_PATH")
      <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT_PATH")
    addCommand "stats" "Collects statistics about BLP images in folder" (uncurry collectStatistics) $ (,)
      <$> strOption (long "input" <> short 'i' <> metavar "DIRECTORY")
      <*> strOption (long "output" <> short 'o' <> metavar "OUTPUT_PATH")
  runCmd
  where 
    ver = "0.1.0.0"
    headerDesc = ""
    desc = "Converting BLP1 Warcraft III format to/from PNG, JPEG, TGA, TIFF, BMP, GIF"